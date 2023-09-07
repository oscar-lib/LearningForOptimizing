package combinator

import oscar.cbls.core.search.{MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}
import oscar.cbls.lib.search.combinators.DynAndThen

import scala.util.Random

/**
 * Implements an epsilon bandit for dynamically selecting neighborhood, in the same manner as proposed in
 * Chmiela, A., Gleixner, A., Lichocki, P., & Pokutta, S. (2023, May).
 * Online Learning for Scheduling MIP Heuristics.
 * In International Conference on Integration of Constraint Programming, Artificial Intelligence, and Operations Research (pp. 114-123). Cham: Springer Nature Switzerland.
 *
 * @param l list of neighborhood to choose from
 */
class EpsilonGreedyBandit(l: List[Neighborhood]) extends AbstractLearningCombinator("EGreedyBandit") {

  private val epsilon: Double = 0.8; // choose the best neighborhood with probability 1-epsilon
  private var t: Int = 0; // number of times the bandit was called to provide the next neighborhood
  private val weights: Array[Double] = Array.fill(l.length)(1.0 / l.length); // weight associated to each neighborhood
  private val wSol = 0.7; // weight for rewardSol
  private val wEff = 0.1; // weight for rewardEff
  private val wSlope = 0.2; // weight for rewardSlope

  private val nSelected: Array[Int] = Array.fill(l.length)(0); // number of time each neighborhood was selected
  private var lastSelectedIdx: Int = -1; // index of the last selected neighborhood

  private var nTriesWithoutSuccess = 0; // number of last calls without any success
  private var maxNTriesWithoutSuccess = 200; // thresholds for the number of last calls without any success
  private var maxSlope = 0.0; // stores (and updates) the maximum slope ever observed
  private var maxRunTimeObserved: Long = 1; // max run time experienced by a neighborhood

  /** The method that provides a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = {
    if (nTriesWithoutSuccess > maxNTriesWithoutSuccess) {
      nTriesWithoutSuccess = 0;
      return None
    }
    t += 1;
    val epsilon_t: Double = epsilon * Math.sqrt(l.length.toDouble / t)
    val proba_t: Double = Random.nextDouble();
    var neighborhood_idx = 0;
    if (proba_t > epsilon_t) {
      neighborhood_idx = weights.zipWithIndex.maxBy(_._1)._2;
    } else {
      val cumulativeWeights = weights.scanLeft(0.0)(_ + _).tail
      // Draw a random number between 0 and 1
      val randomDraw = Random.nextDouble()
      // Find the interval into which the drawn number falls
      neighborhood_idx = cumulativeWeights.indexWhere(randomDraw <= _);
      if (neighborhood_idx == -1)
        neighborhood_idx = weights.length - 1;
    }
    lastSelectedIdx = neighborhood_idx;
    nSelected(neighborhood_idx) += 1;
    Some(l(neighborhood_idx))
  }

  /** The methods that "learns" from the results of the neighborhoods.
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   */
  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    val improved = hasImproved(neighborhood)
    nTriesWithoutSuccess = if (improved) 0 else nTriesWithoutSuccess + 1;
    // updates the max run time observed
    val lastDuration = neighborhood.profiler.commonProfilingData._lastCallDurationNano;
    maxRunTimeObserved = Math.max(maxRunTimeObserved, lastDuration)
    if (nSelected(lastSelectedIdx) == 1) {
      // initialize the average weight
      weights(lastSelectedIdx) = reward(m, neighborhood);
    } else {
      // update average weight
      weights(lastSelectedIdx) = weights(lastSelectedIdx) + (reward(m, neighborhood) - weights(lastSelectedIdx)) / nSelected(lastSelectedIdx);
    }
  }

  /**
   * Tells if the neighborhood has improved the solution or not
   *
   * @param neighborhood neighborhood that may have improved the solution or not
   * @return 1 if the the solution has been improved, 0 otherwise
   */
  def hasImproved(neighborhood: Neighborhood): Boolean = {
    neighborhood.profiler.commonProfilingData._gain > 0
  }

  /**
   * Gives a reward between [0, 1] depending on the search result when executing a neighborhood
   * A returned value close to 1 means a good reward, and close to 0 a bad reward
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   * @return reward associated to the execution of the neighborhood
   */
  def reward(m: SearchResult, neighborhood: Neighborhood): Double = {
    wSol * rewardSol(m, neighborhood) +
    wEff * rewardEff(m, neighborhood) +
    wSlope * rewardSlope(m, neighborhood)
  }

  /**
   * Gives a reward in [0,1] if the solution was improved
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   * @return 1 if the solution was improved, 0 otherwise
   */
  private def rewardSol(m: SearchResult, neighborhood: Neighborhood): Double = {
    m match {
      case NoMoveFound =>
        // nothing was found, which is a bad news. Gives a 0 reward
        0
      case MoveFound(mf) =>
        // returns 1 if the objective has been improved
        if (neighborhood.profiler.commonProfilingData._lastCallGain > 0) {
          1
        } else
          0
    }
  }

  /**
   * Gives a reward in [0,1] punishing the effort it took to execute the neighborhood
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   * @return reward associated to the runtime of the last call of the neighborhood
   */
  private def rewardEff(m: SearchResult, neighborhood: Neighborhood): Double = {
    1 - neighborhood.profiler.commonProfilingData._lastCallDurationNano / maxRunTimeObserved;
  }

  /**
   * Gives a reward in [0, 1] associated to the slope found by the neighborhood
   * 1 means a good slope and 0 a bad one
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   * @return reward associated to the slope found by the neighborhood
   */
  private def rewardSlope(m: SearchResult, neighborhood: Neighborhood): Double = {
    val currentSlope = slope(neighborhood)
    maxSlope = Math.max(maxSlope, currentSlope)
    slope(neighborhood) / maxSlope
  }

  /**
   * Gives the slope (gain over time) from a neighborhood
   *
   * @param neighborhood the neighborhood from which the slope must be computed
   * @return slope from a neighborhood
   */
  private def slope(neighborhood: Neighborhood): Double = {
    var slope = 0.0
    // dear God, forgive me for my sins
    neighborhood match {
      // DynAndThen does not set correctly the profiler information, need to cast to retrieve it correctly
      case n: NeighborhoodCombinator =>
        if (n.subNeighborhoods.length == 1) {
          n.subNeighborhoods.head match {
            case dat: DynAndThen[_] =>
              slope = - (dat.profiler.commonProfilingData.gain * 1000) / Math.max(dat.profiler.commonProfilingData.timeSpentMillis, 1)
            case _ =>
              slope = -(n.profiler.commonProfilingData.gain * 1000) / Math.max(neighborhood.profiler.commonProfilingData.timeSpentMillis, 1)
          }
        } else {
          slope = -(n.profiler.commonProfilingData.gain * 1000) / Math.max(neighborhood.profiler.commonProfilingData.timeSpentMillis, 1)
        }
      case _ =>
        slope = -(neighborhood.profiler.commonProfilingData.gain * 1000) / Math.max(neighborhood.profiler.commonProfilingData.timeSpentMillis, 1);
    }
    slope
  }

}
