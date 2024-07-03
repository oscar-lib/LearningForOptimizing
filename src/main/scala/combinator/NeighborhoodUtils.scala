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

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}
import oscar.cbls.core.search.profiling.CommonProfilingData
import oscar.cbls.lib.search.combinators.DynAndThen

object NeighborhoodUtils {

  def getProfiler(neighborhood: Neighborhood): CommonProfilingData = {
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

  /** Gives the slope (gain over time) of a neighborhood
    *
    * @param neighborhood
    *   the considered neighborhood
    */
  def slope(neighborhood: Neighborhood): Double = {
    val profiler = getProfiler(neighborhood)
    -(profiler.gain * 1000d) / Math.max(profiler.timeSpentMillis, 1)
  }

  /** Gives the total gain of a neighborhood.
    *
    * @param neighborhood
    *   the considered neighborhood
    * @return
    */
  def gain(neighborhood: Neighborhood): Long = {
    getProfiler(neighborhood).gain
  }

  /** Gives the last call gain of a neighborhood.
    *
    * @param neighborhood
    *   the considered neighborhood
    */
  def lastCallGain(neighborhood: Neighborhood): Long = {
    getProfiler(neighborhood)._lastCallGain
  }

  /** Gives the last call duration of a neighborhood, in nanoseconds.
    *
    * @param neighborhood
    *   the considered neighborhood
    */
  def lastCallDuration(neighborhood: Neighborhood): Long = {
    getProfiler(neighborhood)._lastCallDurationNano
  }
}
