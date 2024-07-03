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

import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

import scala.util.Random

/** Implements an epsilon bandit for dynamically selecting neighborhood, in the same manner as
  * proposed in Chmiela, A., Gleixner, A., Lichocki, P., & Pokutta, S. (2023, May). Online Learning
  * for Scheduling MIP Heuristics. In International Conference on Integration of Constraint
  * Programming, Artificial Intelligence, and Operations Research (pp. 114-123). Cham: Springer
  * Nature Switzerland.
  *
  * @param l
  *   list of neighborhood to choose from
  */
class EpsilonGreedyBandit(l: List[Neighborhood])
    extends AbstractLearningCombinator("EGreedyBandit", l: _*) {

  private val epsilon: Double = 0.7 // choose the best neighborhood with probability epsilon
  // (well, almost with epsilon probability, since it changes a bit over time)
  private var t: Int = 0 // number of times the bandit was called to provide the next neighborhood
  private val weights: Array[Double] =
    Array.fill(l.length)(1.0 / l.length) // weight associated to each neighborhood
  private val wSol   = 0.4 // weight for rewardSol
  private val wEff   = 0.2 // weight for rewardEff
  private val wSlope = 0.4 // weight for rewardSlope

  private val nSelected: Array[Int] =
    Array.fill(l.length)(0) // number of time each neighborhood was selected
  private var lastSelectedIdx: Int = -1 // index of the last selected neighborhood

  private var maxSlope                 = 1.0 // stores (and updates) the maximum slope ever observed
  private var maxRunTimeObserved: Long = 1   // max run time experienced by a neighborhood
  private val minWeight =
    (1.0 / l.length) / 25 // min value for the weight, no one will ever go below that

  private val authorizedNeighborhood: Array[Boolean] = Array.fill(l.length)(true)
  private var nTabu                                  = 0

  override def reset(): Unit = {
    if (nTabu != 0) {
      for (i <- authorizedNeighborhood.indices) {
        authorizedNeighborhood(i) = true
      }
      nTabu = 0
    }
    super.reset()
  }

  /** The method that provides a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def getNextNeighborhood: Option[Neighborhood] = {
    if (nTabu == l.length)
      return None // all neighborhoods did not progress
    t += 1
    val epsilon_t: Double = epsilon * Math.sqrt(l.length.toDouble / t)
    val proba_t: Double   = Random.nextDouble()
    var neighborhood_idx  = 0
    if (proba_t > epsilon_t) {
      neighborhood_idx =
        weights.zipWithIndex.filter(idx => authorizedNeighborhood(idx._2)).maxBy(_._1)._2
    } else {
      // sum of valid weights
      val cumulativeWeights = weights.indices
        .scanLeft(0.0)((sum, idx) => if (authorizedNeighborhood(idx)) sum + weights(idx) else sum)
        .tail
      // Draw a random number between 0 and sum of weights
      val randomDraw = Random.nextDouble() * cumulativeWeights.sum
      // Find the interval into which the drawn number falls
      neighborhood_idx = cumulativeWeights.indexWhere(randomDraw <= _)
      if (neighborhood_idx == -1)
        neighborhood_idx = weights.length - 1
    }
    lastSelectedIdx = neighborhood_idx;
    nSelected(neighborhood_idx) += 1
    // println("[" + weights.map(d => f"$d%.2f").mkString(" ") + "] choosing " + lastSelectedIdx + " (" + l(neighborhood_idx) + ")")
    Some(l(neighborhood_idx))
  }

  /** The methods that "learns" from the results of the neighborhoods.
    *
    * @param m
    *   the last search result obtain
    * @param neighborhood
    *   the neighborhood from which the search result has been obtained
    */
  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    updateTabu(m, neighborhood)
    // updates the max run time observed
    val lastDuration = NeighborhoodUtils.lastCallDuration(neighborhood)
    maxRunTimeObserved = Math.max(maxRunTimeObserved, lastDuration)
    val r = reward(m, neighborhood)
    if (nSelected(lastSelectedIdx) == 1) {
      // initialize the average weight
      updateWeight(lastSelectedIdx, r)
    } else {
      // update average weight
      val newWeight =
        weights(lastSelectedIdx) + (r - weights(lastSelectedIdx)) / nSelected(lastSelectedIdx)
      updateWeight(lastSelectedIdx, newWeight)
    }
  }

  /** Updates the weight associated to a neighborhood.
    *
    * The weight can be limited by a given value.
    *
    * @param idx
    *   the neighborhood's index
    * @param value
    *   the limit to the new weight value
    */
  private def updateWeight(idx: Int, value: Double): Unit = {
    weights(idx) = Math.max(value, minWeight)
  }

  /** Updates the tabu list, either adding the neighborhood to the tabu list if it found nothing, or
    * resetting the tabu list otherwise.
    *
    * @param m
    *   the move resulting from the neighborhood
    * @param neighborhood
    *   the considered neighborhood
    */
  private def updateTabu(m: SearchResult, neighborhood: Neighborhood): Unit = {
    m match {
      case NoMoveFound =>
        nTabu += 1
        authorizedNeighborhood(lastSelectedIdx) = false
      case MoveFound(_) =>
        if (nTabu != 0) {
          for (i <- authorizedNeighborhood.indices) {
            authorizedNeighborhood(i) = true
          }
          nTabu = 0
        }
    }
  }

  /** Tells if the neighborhood has improved the solution or not
    *
    * @param neighborhood
    *   neighborhood that may have improved the solution or not
    * @return
    *   1 if the the solution has been improved, 0 otherwise
    */
  def hasImproved(neighborhood: Neighborhood): Boolean = {
    neighborhood.profiler.commonProfilingData._gain > 0
  }

  /** Gives a reward between [0, 1] depending on the search result when executing a neighborhood A
    * returned value close to 1 means a good reward, and close to 0 a bad reward
    *
    * @param m
    *   the last search result obtain
    * @param neighborhood
    *   the neighborhood from which the search result has been obtained
    * @return
    *   reward associated to the execution of the neighborhood
    */
  def reward(m: SearchResult, neighborhood: Neighborhood): Double = {
    wSol * rewardSol(m, neighborhood) +
      wEff * rewardEff(m, neighborhood) +
      wSlope * rewardSlope(m, neighborhood)
  }

  /** Gives a reward in [0,1] if the solution was improved
    *
    * @param m
    *   the last search result obtain
    * @param neighborhood
    *   the neighborhood from which the search result has been obtained
    * @return
    *   1 if the solution was improved, 0 otherwise
    */
  private def rewardSol(m: SearchResult, neighborhood: Neighborhood): Double = {
    m match {
      case NoMoveFound =>
        // nothing was found, which is a bad news. Gives a 0 reward
        0
      case MoveFound(_) =>
        // returns 1 if the objective has been improved
        if (neighborhood.profiler.commonProfilingData._lastCallGain > 0) {
          1
        } else
          0
    }
  }

  /** Gives a reward in [0,1] punishing the effort it took to execute the neighborhood
    *
    * @param m
    *   the last search result obtain
    * @param neighborhood
    *   the neighborhood from which the search result has been obtained
    * @return
    *   reward associated to the runtime of the last call of the neighborhood
    */
  private def rewardEff(m: SearchResult, neighborhood: Neighborhood): Double = {
    val duration = NeighborhoodUtils.lastCallDuration(neighborhood)
    1.0 - duration.toDouble / maxRunTimeObserved
  }

  /** Gives a reward in [0, 1] associated to the slope found by the neighborhood 1 means a good
    * slope and 0 a bad one
    *
    * @param m
    *   the last search result obtain
    * @param neighborhood
    *   the neighborhood from which the search result has been obtained
    * @return
    *   reward associated to the slope found by the neighborhood
    */
  private def rewardSlope(m: SearchResult, neighborhood: Neighborhood): Double = {
    val currentSlope = NeighborhoodUtils.slope(neighborhood)
    maxSlope = Math.max(maxSlope, Math.abs(currentSlope))
    Math.abs(currentSlope / maxSlope)
  }

}
