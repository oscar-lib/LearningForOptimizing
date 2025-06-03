package combinator

import oscar.cbls.core.search.Neighborhood

import scala.collection.mutable

sealed abstract class RewardModel {
  protected var maxSlope: Double           = 1.0 // stores (and updates) the maximum slope ever observed

  def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double

  /** Gives a reward in [0, 1] based on the slope. 0 is the worst slope being found, 1 is the best
   * one
   *
   * @param runStat
   *   statistics from a performed move
   * @return
   *   reward in [0, 1]
   */
  protected def slopeReward(runStat: NeighborhoodStats): Double = {
    val slope = Math.abs(runStat.slope)
    this.maxSlope = Math.max(this.maxSlope, slope)
    slope / maxSlope
    //slope
  }
}

class OriginalRewardModel(
  /** weight rewarding a move being found */
  wSol: Double = 0.4,
  /** weight rewarding small execution time */
  wEff: Double = 0.2,
  /** weight rewarding the slope */
  wSlope: Double = 0.4
) extends NormalizedWindowedSlope(30) {
  private var maxRunTimeNano: Long = 1   // max run time experienced by a neighborhood

  /** Gives a reward in [0, 1] based on finding a move. 1 means that a move was found, 0 otherwise
    *
    * @param runStat
    *   statistics from a performed move
    * @return
    *   reward in [0, 1]
    */
  private def rewardFoundMove(runStat: NeighborhoodStats): Double = {
    if (runStat.foundMove) {
      1.0
    } else {
      0.0
    }
  }

  /** Gives a reward in [0, 1] based on the execution time. 0 means that the execution was the
    * slowest observed, and near 1 values the fastest observed
    *
    * @param runStat
    *   statistics from a performed move
    * @return
    *   reward in [0, 1]
    */
  private def rewardExecutionTime(runStat: NeighborhoodStats): Double = {
    val duration = runStat.timeNano
    1.0 - duration.toDouble / maxRunTimeNano
  }

  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    this.maxSlope = Math.max(this.maxSlope, runStat.slope)
    this.maxRunTimeNano = Math.max(this.maxRunTimeNano, runStat.timeNano)
    this.wSol * rewardFoundMove(runStat) +
      this.wEff * rewardExecutionTime(runStat) +
      this.wSlope * slopeReward(runStat)
  }
}

class SlopeReward extends RewardModel {
  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    slopeReward(runStat)
  }
}

/**
 * Slope reward, normalized by the maximum slope over the last X iterations
 *
 * @param windowSize number of past slopes retained for computing the maximum slope
 */
class NormalizedWindowedSlope(windowSize: Int) extends RewardModel {
  private val window: mutable.Queue[Double] = mutable.Queue.empty

  override def slopeReward(runStat: NeighborhoodStats): Double = {
    val slope = Math.abs(runStat.slope)
    window.enqueue(slope)
    maxSlope = Math.max(maxSlope, slope)
    if (window.size > windowSize) {
      val oldestSlope = window.dequeue()
      if (oldestSlope == maxSlope) {
        maxSlope = window.max
      }
    }
    if (maxSlope == 0) {
      0
    } else {
      slope / maxSlope;
    }
  }

  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    slopeReward(runStat)
  }
}

sealed abstract class NormalizedGain extends RewardModel {

  def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    val profiler = NeighborhoodUtils.getProfiler(neighborhood)
    val gain     = profiler._lastCallGain
    this.update(gain)
    this.normalize(gain)
  }

  protected def update(gain: Long): Unit
  protected def normalize(gain: Long): Double
}

class NormalizedMaxGain extends NormalizedGain {
  protected var maxGain: Long = Long.MinValue

  protected def update(gain: Long): Unit = {
    if (gain > maxGain) {
      maxGain = gain
    }
  }

  protected def normalize(gain: Long): Double = {
    gain.toDouble / maxGain
  }
}

class NormalizedWindowedMaxGain(windowSize: Int) extends NormalizedMaxGain {
  private val window: mutable.Queue[Long] = mutable.Queue.empty
  private var maxIndex                    = 0

  override protected def update(gain: Long): Unit = {
    window.enqueue(gain)
    // Update the current maximal value
    if (gain > window(maxIndex)) {
      maxIndex = window.size - 1
    }
    // Manage the window
    if (window.size > windowSize) {
      window.dequeue()
      maxIndex -= 1
      if (maxIndex < 0) {
        // Recalculate the maximal value if it has gone out of the window
        maxIndex = window.zipWithIndex.maxBy(_._1)._2
      }
    }
    maxGain = window(maxIndex)
  }
}

class NormalizedWindowedMeanGain(windowSize: Int) extends NormalizedGain {
  private val window: mutable.Queue[Long] = mutable.Queue.empty
  private var sum                         = 0L

  override protected def update(gain: Long): Unit = {
    window.enqueue(gain)
    this.sum += gain
    if (window.size > windowSize) {
      val item = window.dequeue()
      this.sum -= item
    }
  }

  override protected def normalize(gain: Long): Double = {
    gain.toDouble / (this.sum.toDouble / window.size)
  }
}


/** Returns the log_10 of the gain of the last move. In the case of negative gains, returns
 * -log_10(-gain) to have a consistent negative reward.
 */
class LogGain extends RewardModel {
  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    val profiler = NeighborhoodUtils.getProfiler(neighborhood)
    if (profiler._lastCallGain == 0) {
      0
    } else if (profiler._lastCallGain > 0) {
      math.log10(profiler._lastCallGain.toDouble)
    } else {
      // The new solution is worse
      -math.log10(-profiler._lastCallGain.toDouble)
    }
  }
}