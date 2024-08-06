// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package combinator

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.profiling.SelectionProfiler
import oscar.cbls.core.search.{
  AcceptanceCriterion,
  MoveFound,
  Neighborhood,
  NeighborhoodCombinator,
  NoMoveFound,
  SearchResult
}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/** Base abstract class for bandit selectors. This handles a tabu list and a weight update based on
  * a learning rate and a learning scheme. Two abstract methods must be implemented:
  * getNextNeighborhood, giving a neighborhood to perform, and reward, computing the reward of a
  * neighborhood. Different methods can also be overridden to enforce particular weighting scheme.
  *
  * @param neighborhoods
  *   neighborhoods available for selection
  * @param learningScheme
  *   when the bandit should learn
  * @param seed
  *   seed used for random number generation
  * @param learningRate
  *   learning rate used to update the weights based on the reward
  */
abstract class BanditSelector(
  neighborhoods: List[Neighborhood],
  learningScheme: LearningScheme = AfterEveryMove,
  seed: Int = 42,
  learningRate: Double = 0.1
) extends NeighborhoodCombinator(neighborhoods: _*) {

  private val _profiler: SelectionProfiler = new SelectionProfiler(this, neighborhoods)

  override def profiler: SelectionProfiler = _profiler

  // number of neighborhoods marked as tabu
  protected var nTabu = 0

  // false if a neighborhood is marked as tabu
  protected val authorizedNeighborhood: Array[Boolean] = Array.fill(neighborhoods.length)(true)

  // weight associated to each neighborhood
  protected val weights: Array[Double] = Array.fill(neighborhoods.length)(1.0)

  // sum of weights of neighborhoods that are not marked as tabu
  protected var sumWeightsValidNeighborhoods: Double = neighborhoods.length

  // for each neighborhood, its index in the list
  protected var neighborhoodIdx: mutable.HashMap[Neighborhood, Int] = mutable.HashMap.empty

  // list of stats for each neighborhood. One stat is collected per move
  protected var stats: Array[ListBuffer[NeighborhoodStats]] =
    Array.fill(neighborhoods.length)(ListBuffer())
  protected val rand = new Random(seed)

  // number of time each neighborhood was selected
  protected val nSelected: Array[Int] = Array.fill(neighborhoods.length)(0)

  // index of the last selected neighborhood
  protected var lastSelectedIdx: Int = -1

  // Populate the map
  for (i <- neighborhoods.indices) {
    neighborhoodIdx += (neighborhoods(i) -> i)
  }

  protected var maxSlope             = 1.0 // stores (and updates) the maximum slope ever observed
  protected var maxRunTimeNano: Long = 1   // max run time experienced by a neighborhood

  // TODO next steps to slightly speeds the selection:
  //  1. use sparse-set to maintain the neighborhoods that are not marked as tabu
  //  2. use sparse-set to maintain the neighborhoods that have been selected at least once since the last weight update (only them must have their weight updated)

  /** The method that provides a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  def getNextNeighborhood: Option[Neighborhood]

  /** The method that computes a reward associated to a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  def reward(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double

  /** Resets the selector. This resets the tabu list and update the weights if the learning scheme
    * is set to
    *   - after every descent.
    *   - after n moves and the current.
    */
  override def reset(): Unit = {
    resetTabu()
    learningScheme match {
      case AfterEveryDescent =>
        updateWeights()
        clearStats()
      case afterNMoves @ AfterNMoves(_) =>
        if (afterNMoves.nMoves > 0) {
          updateWeights()
          afterNMoves.resetCounter()
          clearStats()
        }
      case AfterEveryMove =>
    }
  }

  /** Clears the stats associated to all neighborhoods.
    */
  private def clearStats(): Unit = {
   stats.foreach(_.clear())
    }
  }

  /** Clears the stats associated to one specific neighborhood.
    *
    * @param neighborhood
    *   neighborhood on which the stats must be cleared.
    */
  private def clearStats(neighborhood: Neighborhood): Unit = {
    val idx = neighborhoodIdx(neighborhood)
    stats(idx).clear()
  }

  /** Tells that a move has been performed, and update the tabu list and weights if necessary.
    *
    * @param searchResult
    *   results of performing the move.
    * @param neighborhood
    *   move performed.
    */
  def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    val stats = NeighborhoodStats(searchResult, neighborhood)
    appendStats(stats, neighborhood)
    searchResult match {
      case NoMoveFound  => setTabu(neighborhood)
      case MoveFound(_) =>
    }
    learningScheme match {
      case AfterEveryMove =>
        updateWeight(neighborhood)
        clearStats(neighborhood)
      case afterNMoves @ AfterNMoves(_) =>
        afterNMoves.incrementCounter()
        if (afterNMoves.isCriterionMet) {
          updateWeights()
          afterNMoves.resetCounter()
          clearStats(neighborhood)
        }
      case AfterEveryDescent =>
    }
  }

  /** Add a newly collected statistic
    *
    * @param neighborhoodStats
    *   statistics about a move
    * @param neighborhood
    *   neighborhood having performed the move
    */
  protected def appendStats(
                             neighborhoodStats: NeighborhoodStats,
                             neighborhood: Neighborhood
  ): Unit = {
    maxSlope = Math.max(maxSlope, neighborhoodStats.slope)
    maxRunTimeNano = Math.max(maxRunTimeNano, neighborhoodStats.timeNano)
    val idx = neighborhoodIdx(neighborhood)
    stats(idx).append(neighborhoodStats)
  }

  object authorizedNeighborhoodIterator {
    def apply(): Iterator[Int] = {
      authorizedNeighborhood.indices.iterator.filter(authorizedNeighborhood(_))
    }
  }

  /** Mark a neighborhood as tabu This has the side-effect of decreasing the sum of valid weights
    *
    * @param neighborhood
    *   neighborhood to mark as tabu
    */
  def setTabu(neighborhood: Neighborhood): Unit = {
    val idx = neighborhoodIdx(neighborhood)
    authorizedNeighborhood(idx) = false
    sumWeightsValidNeighborhoods -= weights(idx)
    nTabu += 1
  }

  def isTabu(neighborhood: Neighborhood): Boolean = {
    val idx = neighborhoodIdx(neighborhood)
    authorizedNeighborhood(idx)
  }

  def isTabu(idx: Int): Boolean = {
    authorizedNeighborhood(idx)
  }

  /** Reset the list of tabu neighborhoods and update the sum of weights for valid neighborhoods
    */
  private def resetTabu(): Unit = {
    if (nTabu != 0) {
      for (idx <- authorizedNeighborhood.indices) {
        authorizedNeighborhood(idx) = true
        sumWeightsValidNeighborhoods += weights(idx)
      }
    }
    nTabu = 0
  }

  /** Tells the probability of choosing a neighborhood, proportional to its weight.
    *
    * A neighborhood set as tabu has a probability equal to 0.
    *
    * @param neighborhood
    *   the considered neighborhood
    */
  def probability(neighborhood: Neighborhood): Double = {
    val idx = neighborhoodIdx(neighborhood)
    if (authorizedNeighborhood(idx)) {
      weights(idx) / sumWeightsValidNeighborhoods
    } else {
      0
    }
  }

  /** Tells the weight associated to a neighborhood. There are no bounds set on the weight: it can
    * be larger than 1.
    *
    * @param neighborhood
    *   the considered neighborhood
    */
  def weight(neighborhood: Neighborhood): Double = {
    val idx = neighborhoodIdx(neighborhood)
    this.weights(idx)
  }

  /** Returns the neighborhood with the best weight, if applicable.
    */
  def getBestNeighborhood: Option[Neighborhood] = {
    // (weight, idx) of the neighborhood with maximum weight
    if (nTabu == neighborhoods.length)
      None
    else {
      val best = weights.zipWithIndex.filter(i => authorizedNeighborhood(i._2)).maxBy(i => i._1)
      Some(neighborhoods(best._2))
    }
  }

  /** Returns a neighborhood with a probability proportional to its weight. The higher the weight of
    * a neighborhood, the larger the probability of it being returned
    *
    * @return
    */
  def getNeighborhoodWithProbability: Option[Neighborhood] = {
    if (nTabu == neighborhoods.length)
      None // no neighborhood still valid
    else {
      var prob = rand.nextDouble() * sumWeightsValidNeighborhoods
      for ((weight, idx) <- weights.zipWithIndex) {
        if (authorizedNeighborhood(idx)) {
          if (prob < weight) {
            return Some(neighborhoods(idx))
          }
          prob -= weight // invalid neighborhood, try the next one
        }
      }
      Some(neighborhoods.last)
    }
  }

  /** Gives a random neighborhood in within the available ones
    *
    * @return
    *   random neighborhood being not tabu
    */
  def getRandomNeighborhood: Option[Neighborhood] = {
    if (nTabu == neighborhoods.length) {
      None
    } else {
      var prob = rand.nextInt(neighborhoods.length - nTabu)
      for (idx <- neighborhoods.indices) {
        if (authorizedNeighborhood(idx)) {
          if (prob == 0) {
            return Some(neighborhoods(idx))
          }
          prob -= 1 // invalid neighborhood, try the next one
        }
      }
      Some(neighborhoods.last)
    }
  }

  /** Gives a reward in [0, 1] based on finding a move. 1 means that a move was found, 0 otherwise
    *
    * @param runStat
    *   statistics from a performed move
    * @return
    *   reward in [0, 1]
    */
  def rewardFoundMove(runStat: NeighborhoodStats): Double = {
    if (runStat.foundMove) {
      1.0
    } else {
      0.0
    }
  }

  /** Gives a reward in [0, 1] based on the slope. 0 is the worst slope being found, 1 is the best
    * one
    *
    * @param runStat
    *   statistics from a performed move
    * @return
    *   reward in [0, 1]
    */
  def rewardSlope(runStat: NeighborhoodStats): Double = {
    val slope = runStat.slope
    Math.abs(slope / maxSlope)
  }

  /** Gives a reward in [0, 1] based on the execution time. 1 means that the execution was the
    * slowest observed, and near 0 values the fastest observed
    *
    * @param runStat
    *   statistics from a performed move
    * @return
    *   reward in [0, 1]
    */
  def rewardExecutionTime(runStat: NeighborhoodStats): Double = {
    val duration = runStat.timeNano
    1.0 - duration.toDouble / maxRunTimeNano
  }

  /** Update the weights of all neighborhoods registered
    */
  def updateWeights(): Unit = {
    for (neigh <- neighborhoods) {
      updateWeight(neigh)
    }
  }

  /** Update the weight associated to a neighborhood.
    *
    * The weight update is computed based on the collected stats.
    *
    * @param neighborhood
    *   the considered neighborhood
    */
  def updateWeight(neighborhood: Neighborhood): Unit = {
    val idx = neighborhoodIdx(neighborhood)
    if (stats(idx).nonEmpty) {
      val aggregatedReward = if (stats(idx).length == 1) {
        reward(stats(idx).last, neighborhood)
      } else {
        val rewardsOnEpisode = stats(idx).map(stat => reward(stat, neighborhood))
        aggregate(rewardsOnEpisode, neighborhood)
      }
      val oldWeight    = weights(idx)
      val newWeight    = newWeightFromReward(neighborhood, oldWeight, aggregatedReward)
      val weightChange = newWeight - oldWeight
      if (authorizedNeighborhood(idx)) {
        sumWeightsValidNeighborhoods += weightChange
      }
      weights(idx) = newWeight
    }
  }

  /** Gives the new values to set for the weight of a neighborhood
    *
    * @param neighborhood
    *   neighborhood on which the reward has been computed
    * @param oldWeight
    *   old weight of the neighborhood
    * @param reward
    *   reward associated to the neighborhood
    * @return
    *   new weight to set for the neighborhood
    */
  def newWeightFromReward(neighborhood: Neighborhood, oldWeight: Double, reward: Double): Double = {
    oldWeight + learningRate * reward
  }

  /** Combine multiple rewards into a single one by taking the average of rewards This is useful if
    * the learning mode is not set to be learning after every move
    *
    * @param rewards
    *   list of rewards that must be combined
    * @param neighborhood
    *   neighborhood on which the rewards are associated
    * @return
    */
  def aggregate(rewards: ListBuffer[Double], neighborhood: Neighborhood): Double = {
    rewards.sum / rewards.length
  }

  override def getMove(
    obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion
  ): SearchResult = {

    @tailrec
    def doSearch(): SearchResult = {
      if (nTabu == neighborhoods.length) {
        NoMoveFound
      } else {
        getNextNeighborhood match {
          case None => NoMoveFound
          case Some(n) =>
            val idx = neighborhoodIdx(n)
            lastSelectedIdx = idx
            nSelected(idx) += 1
            val candidateResult = n.getProfiledMove(obj, initialObj, acceptanceCriterion)
            notifyMove(candidateResult, n)
            candidateResult match {
              case NoMoveFound => doSearch()
              case MoveFound(_) => candidateResult
            }
        }
      }
    }

    doSearch()
  }

}
