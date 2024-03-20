package combinator
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * An epsilon greedy bandit that learns (i.e. updates its probabilities) only after X neighborhoods have been tried
 */
class WindowedEpsilonGreedyBandit(l: List[Neighborhood], window: Int = 500) extends AbstractLearningCombinator(name = "WindowedEpsilonGreedy") {

  private val epsilon: Double = 0.7; // choose the best neighborhood with probability epsilon
  protected var neighStatistics: Array[ListBuffer[RunStat]] = Array.fill(l.length)(ListBuffer())
  protected var t: Long = 0 // number of calls done to the bandit, asking for a next neighborhood
  private var currentIndex = -1  // index of the last neighborhood that has been tried
  private val wSol = 0.4; // weight for rewardSol
  private val wEff = 0.2; // weight for rewardEff
  private val wSlope = 0.4; // weight for rewardSlope
  private var maxRunTimeObserved: Long = 1; // max run time experienced by a neighborhood
  private val weights: Array[Double] = Array.fill(l.length)(1.0 / l.length); // weight associated to each neighborhood
  private val nSelected: Array[Int] = Array.fill(l.length)(0); // number of time each neighborhood was selected
  private var maxSlope = 1.0; // stores (and updates) the maximum slope ever observed
  private var minWeight = (1.0 / l.length) / 25; // min value for the weight, no one will ever go below that

  private var authorizedNeighborhood: Array[Boolean] = Array.fill(l.length)(true)
  private var nTabu = 0;


  override def reset(): Unit = {
    updateWeights()
    resetEpisode()
    if (nTabu != 0) {
      for (i <- authorizedNeighborhood.indices) {
        authorizedNeighborhood(i) = true
      }
      nTabu = 0;
    }
    super.reset()
  }

  /** The method that provides a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = {
    if (nTabu == l.length) {
      return None // all neighborhoods did not progress
    }
    t += 1;
    val epsilon_t: Double = epsilon * Math.sqrt(l.length.toDouble / t)
    val proba_t: Double = Random.nextDouble();
    var neighborhood_idx = 0;
    if (proba_t > epsilon_t) {
      neighborhood_idx = weights.zipWithIndex.filter(idx => authorizedNeighborhood(idx._2)).maxBy(_._1)._2;
    } else {
      // sum of valid weights
      val cumulativeWeights = weights.indices.scanLeft(0.0)(
        (sum, idx) => if (authorizedNeighborhood(idx)) sum + weights(idx) else sum).tail
      // Draw a random number between 0 and sum of weights
      val randomDraw = Random.nextDouble() * cumulativeWeights.sum
      // Find the interval into which the drawn number falls
      neighborhood_idx = cumulativeWeights.indexWhere(randomDraw <= _);
      if (neighborhood_idx == -1)
        neighborhood_idx = weights.length - 1;
    }
    currentIndex = neighborhood_idx;
    nSelected(neighborhood_idx) += 1;
    //println("[" + weights.map(d => f"$d%.2f").mkString(" ") + "] choosing " + lastSelectedIdx + " (" + l(neighborhood_idx) + ")")
    Some(l(neighborhood_idx))
  }

  /** The methods that "learns" from the results of the neighborhoods.
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   */
  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    updateTabu(m, neighborhood)
    // updates the max run time observed
    val lastDuration = NeighborhoodUtils.lastCallDuration(neighborhood);
    maxRunTimeObserved = Math.max(maxRunTimeObserved, lastDuration)
    // add to the statistics
    updateStatistics(m, neighborhood)
    // if the statistics have reached a fixed length, update the weights based on them and reset them
    if (t % window == 0) {
      updateWeights()
      resetEpisode()
    }
  }

  /**
   * Updates the tabu list.
   * Either adds the neighborhood to the tabu list if it found nothing
   * Otherwise, resets the tabu list
   *
   * @param m
   * @param neighborhood
   */
  private def updateTabu(m: SearchResult, neighborhood: Neighborhood): Unit = {
    m match {
      case NoMoveFound =>
        nTabu += 1;
        authorizedNeighborhood(currentIndex) = false
      case MoveFound(_) =>
        if (nTabu != 0) {
          for (i <- authorizedNeighborhood.indices) {
            authorizedNeighborhood(i) = true
          }
          nTabu = 0;
        }
    }
  }

  /**
   * Updates the statistics based on a run with a given neighborhood
   * @param m
   * @param neighborhood
   */
  private def updateStatistics(m: SearchResult, neighborhood: Neighborhood): Unit = {
    neighStatistics(currentIndex).append(RunStat(m, neighborhood))
  }

  /**
   * Resets the statistics that have been stored
   */
  private def resetEpisode(): Unit = {
    for (i <- neighStatistics.indices) {
      neighStatistics(i).clear()
    }
  }

  /**
   * Updates the weights from the bandit using the collected statistics
   */
  private def updateWeights(): Unit = {
    // computes the reward of the episode
    val currentRewards = episodeReward()
    // updates the weight based on the reward
    for (i <- currentRewards.indices) {
      val r = currentRewards(i)
      r match {
        case Some(value) => {
          if (nSelected(currentIndex) == 1) {
            // initialize the average weight
            updateWeight(currentIndex, value)
          } else {
            // update average weight
            val newWeight = weights(currentIndex) + (value - weights(currentIndex)) / nSelected(currentIndex);
            updateWeight(currentIndex, newWeight)
          }
        }
        case None => ; // nothing do to if no reward was associated to the neighborhood
      }
    }
  }

  /**
   * Updates the weight associated to a neighborhood
   * The weight cannot go beyond a given value
   *
   * @param idx
   * @param value
   */
  private def updateWeight(idx: Int, value: Double): Unit = {
    weights(idx) = Math.max(value, minWeight)
  }

  /**
   * Gets the array of current rewards
   * @return
   */
  private def episodeReward(): Array[Option[Double]] = {
    // computes the sum of rewards
    var sumReward: Double = 0
    val nNeighborCalls : Array[Int] = Array.fill(l.length)(0) // tracks the number of times a neighborhood was called
    for (i <- neighStatistics.indices) {
      sumReward += neighStatistics(i).map(r => reward(r)).sum
      nNeighborCalls(i) = neighStatistics(i).length
    }
    val sumNNeighborCalls = nNeighborCalls.sum
    // distribute the sum of reward according to the number of calls
    val rewards: Array[Option[Double]] = Array.fill(l.length)(None)
    for (i <- nNeighborCalls.indices) {
      if (nNeighborCalls(i) > 0)
        rewards(i) = Some(sumReward / sumNNeighborCalls * nNeighborCalls(i))
    }
    rewards
  }

  private def reward(runStat: RunStat): Double = {
    wSol * rewardSol(runStat) +
      wEff * rewardEff(runStat) +
      wSlope * rewardSlope(runStat)
  }

  private def rewardSlope(runStat: RunStat): Double = {
    val currentSlope = runStat.slope
    maxSlope = Math.max(maxSlope, Math.abs(currentSlope))
    Math.abs(currentSlope / maxSlope)
  }

  private def rewardSol(runStat: RunStat): Double = {
    if (runStat.hasImproved) 1 else 0
  }

  private def rewardEff(runStat: RunStat): Double = {
    val duration = runStat.timeNano
    1.0 - duration.toDouble / maxRunTimeObserved
  }
}

case class RunStat(
  hasImproved: Boolean,
  noMoveFound: Boolean,
  slope: Double,
  timeNano: Long,
)

object RunStat {
  def apply(m: SearchResult, neighborhood: Neighborhood): RunStat = {
    new RunStat(
      neighborhood.profiler.commonProfilingData._gain > 0,
      m match {
        case NoMoveFound => true
        case MoveFound(m) => false
      },
      NeighborhoodUtils.slope(neighborhood),
      NeighborhoodUtils.lastCallDuration(neighborhood)
    )
  }
}