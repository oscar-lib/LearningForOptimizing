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

/** Lightweight store of information regarding the execution of a move
  *
  * @param hasImproved
  * @param foundMove
  * @param slope
  * @param timeNano
  */
case class NeighorhoodStats(hasImproved: Boolean, foundMove: Boolean, slope: Double, timeNano: Long)

object NeighorhoodStats {

  /** Create a neighborhood statistics based on a move performed
    * @param searchResult
    *   results of performing a move
    * @param neighborhood
    *   move performed
    * @return
    *   statistics based on the move performed
    */
  def apply(searchResult: SearchResult, neighborhood: Neighborhood): NeighorhoodStats = {
    NeighorhoodStats(
      searchResult match {
        case NoMoveFound  => false
        case MoveFound(_) => true
      },
      neighborhood.profiler.commonProfilingData._gain > 0,
      NeighborhoodUtils.slope(neighborhood),
      NeighborhoodUtils.lastCallDuration(neighborhood)
    )
  }
}
