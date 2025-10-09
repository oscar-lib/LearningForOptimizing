package combinator

import oscar.cbls.core.search.Neighborhood

import scala.collection.mutable

sealed abstract class RewardModel(normalizationFactor: Float) {
  protected var maxSlope: Double = 1.0 // stores (and updates) the maximum slope ever observed

  def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double
  def apply(prevObj: Long, newObj: Long): Double

  def transformObjective(obj: Long): Double = obj.toDouble * normalizationFactor

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
    // slope
  }
}

class OriginalRewardModel(
  /** weight rewarding a move being found */
  wSol: Double = 0.4,
  /** weight rewarding small execution time */
  wEff: Double = 0.2,
  /** weight rewarding the slope */
  wSlope: Double = 0.4,
  slopeWidth: Int = 30
) extends NormalizedWindowedSlope(slopeWidth) {
  private var maxRunTimeNano: Long = 1 // max run time experienced by a neighborhood

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

  override def apply(prevObj: Long, newObj: Long): Double = {
    throw new UnsupportedOperationException(
      "This method is not supported in OriginalRewardModel. Use apply(NeighborhoodStats, Neighborhood) instead."
    )
  }
}

class SlopeReward extends RewardModel(1.0f) {
  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    slopeReward(runStat)
  }

  override def apply(prevObj: Long, newObj: Long): Double = {
    throw new UnsupportedOperationException(
      "This method is not supported in SlopeReward. Use apply(NeighborhoodStats, Neighborhood) instead."
    )
  }
}

/** Slope reward, normalized by the maximum slope over the last X iterations
  *
  * @param windowSize
  *   number of past slopes retained for computing the maximum slope
  */
class NormalizedWindowedSlope(windowSize: Int) extends RewardModel(1.0f) {
  private val window: mutable.Queue[Double] = mutable.Queue.empty // only hold non zero slope

  override def slopeReward(runStat: NeighborhoodStats): Double = {
    val slope = Math.abs(runStat.slope)
    if (slope == 0) // slope of zero are ignored
      return 0;
    window.enqueue(slope)
    maxSlope = Math.max(maxSlope, slope)
    if (window.size > windowSize) {
      val oldestSlope = window.dequeue()
      if (oldestSlope == maxSlope) {
        maxSlope = window.max
      }
    }
    // println("current slope = " + slope + " maxslope = " + maxSlope)
    if (maxSlope == 0) {
      0
    } else {
      slope / maxSlope;
    }
  }

  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    slopeReward(runStat)
  }

  override def apply(prevObj: Long, newObj: Long): Double = {
    throw new UnsupportedOperationException(
      "This method is not supported in SlopeReward. Use apply(NeighborhoodStats, Neighborhood) instead."
    )
  }
}

sealed abstract class NormalizedGain extends RewardModel(1.0f) {

  def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    val profiler = NeighborhoodUtils.getProfiler(neighborhood)
    val gain     = if (runStat.foundMove) profiler._lastCallGain else 0
    this.update(gain)
    this.normalize(gain)
  }

  override def apply(prevObj: Long, newObj: Long): Double = {
    throw new UnsupportedOperationException(
      "This method is not supported in NormalizedGain. Use apply(NeighborhoodStats, Neighborhood) instead."
    )
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
class LogGain(normalizationFactor: Float) extends RewardModel(normalizationFactor) {

  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    val profiler = NeighborhoodUtils.getProfiler(neighborhood)
    if (!runStat.foundMove) {
      0
    } else if (profiler._lastCallGain > 0) {
      math.log10(profiler._lastCallGain.toDouble * normalizationFactor)
    } else {
      // The new solution is worse
      -math.log10(-profiler._lastCallGain.toDouble * normalizationFactor)
    }
  }

  override def apply(prevObj: Long, newObj: Long): Double = {
    if (newObj == prevObj) {
      return 0.0
    }
    val gain = newObj - prevObj
    if (gain < 0) {
      // The new solution is worse
      return -math.log10(-gain.toDouble)
    }
    return math.log10(gain.toDouble)
  }

  override def transformObjective(obj: Long): Double = math.log10(obj.toDouble)
}

/** Difference in objective from the previous solution to the new one.
  */
class Gain(normalizationFactor: Float) extends RewardModel(normalizationFactor) {
  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    if (runStat.foundMove) {
      val profiler = NeighborhoodUtils.getProfiler(neighborhood)
      profiler._lastCallGain.toDouble * normalizationFactor
    } else {
      0
    }

  }

  override def apply(prevObj: Long, newObj: Long): Double = {
    return (prevObj - newObj).toDouble * normalizationFactor
  }
}

/** K. Li, Á. Fialho, S. Kwong, and Q. Zhang. 2014. Adaptive Operator Selection With Bandits for a
  * Multiobjective Evolutionary Algorithm Based on Decomposition. IEEE Transactions on Evolutionary
  * Computation 18, 1 (2014)
  */
class FitnessRateRank extends RewardModel(1.0f) {
  override def apply(prevObj: Long, newObj: Long): Double = {
    return (prevObj - newObj).toDouble / prevObj.toDouble
  }

  override def apply(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    throw new UnsupportedOperationException(
      "This method is not supported in FitnessRateRank. Use apply(prevObj: Long, newObj: Long) instead."
    )
  }
}
