package combinator

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}
import oscar.cbls.core.search.profiling.CommonProfilingData
import oscar.cbls.lib.search.combinators.DynAndThen

object NeighborhoodUtils {

  def getProfiler(neighborhood : Neighborhood) : CommonProfilingData = {
    neighborhood match {
      // DynAndThen does not set correctly the profiler information, need to cast to retrieve it correctly
      case n: NeighborhoodCombinator =>
        if (n.subNeighborhoods.length == 1) {
          n.subNeighborhoods.head match {
            case dat: DynAndThen[_] =>
              dat.profiler.commonProfilingData
            case _ =>
              n.profiler.commonProfilingData
          }
        } else {
          n.profiler.commonProfilingData
        }
      case _ =>
        neighborhood.profiler.commonProfilingData
    }
  }

  /**
   * Gives the slope (gain over time) of a neighborhood
   *
   * @param neighborhood
   * @return
   */
  def slope(neighborhood : Neighborhood) : Double = {
    val profiler = getProfiler(neighborhood)
    - (profiler.gain * 1000) / Math.max(profiler.timeSpentMillis, 1)
  }

  /**
   * Gives the total gain of a neighborhood
   *
   * @param neighborhood
   * @return
   */
  def gain(neighborhood: Neighborhood): Long = {
    getProfiler(neighborhood).gain
  }

  /**
   * Gives the last call gain of a neighborhood
   *
   * @param neighborhood
   * @return
   */
  def lastCallGain(neighborhood: Neighborhood) : Long = {
    getProfiler(neighborhood)._lastCallGain
  }

  /**
   * Gives the last call duration of a neighborhood, in nano seconds
   *
   * @param neighborhood
   * @return
   */
  def lastCallDuration(neighborhood: Neighborhood) : Long = {
    getProfiler(neighborhood)._lastCallDurationNano
  }


}
