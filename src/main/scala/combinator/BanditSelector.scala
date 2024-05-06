package combinator

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.profiling.SelectionProfiler
import oscar.cbls.core.search.{AcceptanceCriterion, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * This class stores the history of previous moved performed and aggregate multiple information
 */
abstract class History(neighborhoods: List[Neighborhood],
                       learningScheme: LearningScheme = AfterEveryMove,
                       seed: Int = 42,
                       learningRate: Double = 0.1) extends NeighborhoodCombinator(neighborhoods: _*) {

  private val _profiler: SelectionProfiler = new SelectionProfiler(this, neighborhoods.toList)
  override def profiler: SelectionProfiler = _profiler

  private var nTabu = 0; // number of neighborhoods marked as tabu
  private val authorizedNeighborhood: Array[Boolean] = Array.fill(neighborhoods.length)(true) // false if a neighborhood is marked as tabu

  private val weights: Array[Double] = Array.fill(neighborhoods.length)(1.0) // weight associated to each neighborhood
  private var sumWeightsValidNeighborhoods: Double = neighborhoods.length // sum of weights of neighborhoods that are not marked as tabu
  private var neighborhoodIdx: mutable.HashMap[Neighborhood, Int] = mutable.HashMap.empty // for each neighborhood, its index in the list

  // list of stats for each neighborhood. One stat is collected per move
  private var stats: Array[ListBuffer[NeighorhoodStats]] = Array.fill(neighborhoods.length)(ListBuffer())
  private val rand = new Random(seed)

  private val nSelected: Array[Int] = Array.fill(neighborhoods.length)(0) // number of time each neighborhood was selected
  private var lastSelectedIdx: Int = -1 // index of the last selected neighborhood

  // Populate the map
  for (i <- neighborhoods.indices) {
    neighborhoodIdx += (neighborhoods(i) -> i)
  }

  private var maxSlope = 1.0 // stores (and updates) the maximum slope ever observed
  private var maxRunTimeNano: Long = 1 // max run time experienced by a neighborhood

  // TODO next steps to slightly speeds the selection:
  //  1. use sparse-set to maintain the neighborhoods that are not marked as tabu
  //  2. use sparse-set to maintain the neighborhoods that have been selected at least once since the last weight update (only them must have their weight updated)

  /**
   * Reset the selector
   * This resets the tabu list and update the weights if the learning scheme is set to
   * - after every descent
   * - after n moves and the current
   */
  override def reset(): Unit = {
    resetTabu()
    learningScheme match {
      case AfterEveryDescent => updateWeights()
      case afterNMoves@AfterNMoves(_) =>
        if (afterNMoves.nMoves > 0) {
          updateWeights()
          afterNMoves.resetCounter();
        }
    }
    for (idx <- stats.indices) {
      stats(idx).clear()
    }
  }

  /**
   * Tells that a move has been performed, and update the tabu list and weights if necessary
   * @param searchResult results of performing the move
   * @param neighborhood move performed
   */
  def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    val stats = NeighorhoodStats(searchResult, neighborhood);
    appendStats(stats, neighborhood)
    searchResult match {
      case NoMoveFound => setTabu(neighborhood)
      case MoveFound(_) =>
    }
    learningScheme match {
      case AfterEveryMove => updateWeight(neighborhood)
      case afterNMoves@AfterNMoves(_) =>
        afterNMoves.incrementCounter()
        if (afterNMoves.isCriterionMet()) {
          updateWeights()
          afterNMoves.resetCounter();
        }
      case AfterEveryDescent =>
    }
  }

  /**
   * Add a newly collected statistic
   * @param neighborhoodStats statistics about a move
   * @param neighborhood neighborhood having performed the move
   */
  protected def appendStats(neighborhoodStats: NeighorhoodStats, neighborhood: Neighborhood): Unit = {
    maxSlope = Math.max(maxSlope, neighborhoodStats.slope)
    maxRunTimeNano = Math.max(maxRunTimeNano, neighborhoodStats.timeNano)
    val idx = neighborhoodIdx(neighborhood)
    stats(idx).append(neighborhoodStats)
  }

  /**
   * Mark a neighborhood as tabu
   * This has the side-effect of decreasing the sum of valid weights
   * @param neighborhood
   */
  def setTabu(neighborhood: Neighborhood): Unit = {
    val idx = neighborhoodIdx(neighborhood)
    authorizedNeighborhood(idx) = false
    sumWeightsValidNeighborhoods -= weights(idx)
    nTabu += 1;
  }

  /**
   * Reset the list of tabu neighborhoods and update the sum of weights for valid neighborhoods
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

  /**
   * Tells the probability of choosing a neighborhood, proportional to its weight
   * A neighborhood set as tabu has a 0-probability of being chosen
   *
   * @param neighborhood
   * @return
   */
  def probability(neighborhood: Neighborhood): Double = {
    val idx = neighborhoodIdx(neighborhood)
    if (authorizedNeighborhood(idx)) {
      weights(idx) / sumWeightsValidNeighborhoods
    } else {
      0
    }
  }

  /**
   * Tells the weight associated to a neighborhood
   * There are no bounds set on the weight
   *
   * @param neighborhood
   * @return
   */
  def weight(neighborhood: Neighborhood): Double = {
    val idx = neighborhoodIdx(neighborhood)
    this.weights(idx)
  }

  /**
   * Gives the neighborhood having the best weight
   *
   * @return
   */
  def getBestNeighborhood(): Option[Neighborhood] = {
    // (weight, idx) of the neighborhood with maximum weight
    val best = weights.zipWithIndex.filter(i => authorizedNeighborhood(i._2)).maxBy(i => i._1)
    if (best != null) {
      Some(neighborhoods(best._2))
    } else {
      None
    }
  }

  /**
   * Returns a neighborhood with a probability proportional to its weight
   * The higher the weight of a neighborhood, the larger the probability of it being returned
   *
   * @return
   */
  def getNeighborhoodWithProba(): Option[Neighborhood] = {
    if (nTabu == neighborhoods.length)
      None // no neighborhood still valid
    else {
      var proba = rand.nextDouble() * sumWeightsValidNeighborhoods;
      for ((weight, idx) <- weights.zipWithIndex) {
        if (authorizedNeighborhood(idx)) {
          if (proba < weight) {
            return Some(neighborhoods(idx))
          }
          proba -= weight // invalid neighborhood, try the next one
        }
      }
      Some(neighborhoods.last)
    }
  }

  def getRandomNeighborhood(): Option[Neighborhood] = {
    if (nTabu == neighborhoods.length) {
      None
    } else {
      var proba = rand.nextInt(neighborhoods.length - nTabu)
      for (idx <- neighborhoods.indices) {
        if (authorizedNeighborhood(idx)) {
          if (proba == 0) {
            return Some(neighborhoods(idx))
          }
          proba -= 1 // invalid neighborhood, try the next one
        }
      }
      Some(neighborhoods.last)
    }
  }

  def rewardFoundMove(runStat: NeighorhoodStats): Double = {
    if (runStat.foundMove) {
      1.0
    } else {
      0.0
    }
  }

  def rewardSlope(runStat: NeighorhoodStats): Double = {
    val slope = runStat.slope;
    Math.abs(slope / maxSlope)
  }

  def rewardExecutionTime(runStat: NeighorhoodStats): Double = {
    val duration = runStat.timeNano;
    1.0 - duration.toDouble / maxRunTimeNano
  }

  def updateWeights(): Unit = {
    for (neigh <- neighborhoods) {
      updateWeight(neigh)
    }
  }

  /**
   * Update the weight associated to a neighborhood
   * The weight update is computed based on the stats collected
   * @param neighborhood
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
      val newWeight = weights(idx) + learningRate * aggregatedReward
      val weightChange = newWeight - weights(idx)
      if (authorizedNeighborhood(idx)) {
        sumWeightsValidNeighborhoods += weightChange
      }
      weights(idx) = newWeight
    }
  }

  /** The method that provides a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  def getNextNeighborhood: Option[Neighborhood]

  /** The method that computes a reward associated to a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  def reward(runStat: NeighorhoodStats, neighborhood: Neighborhood): Double

  /**
   * Combine multiple rewards into a single one by taking the average of rewards
   * This is useful if the learning mode is not set to be learning after every move
   * @param rewards list of rewards that must be combined
   * @param neighborhood neighborhood on which the rewards are associated
   * @return
   */
  def aggregate(rewards: ListBuffer[Double], neighborhood: Neighborhood): Double = {
    rewards.sum / rewards.length
  }

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {

    @tailrec
    def doSearch(): SearchResult = {
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

    doSearch()
  }

}
