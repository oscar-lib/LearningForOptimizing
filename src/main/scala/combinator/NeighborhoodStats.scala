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
  * @param foundMove
  *   whether a move has been found or not
  * @param hasImproved
  *   whether the move has improved the solution quality or not
  * @param slope
  *   the slope, i.e., the ratio between the quality of improvement and its performance cost
  * @param timeNano
  *   the elapsed time
  */
case class NeighborhoodStats(
  foundMove: Boolean,
  hasImproved: Boolean,
  slope: Double,
  timeNano: Long
)

object NeighborhoodStats {

  /** Create a neighborhood statistics based on a move performed
    * @param searchResult
    *   results of performing a move
    * @param neighborhood
    *   move performed
    * @return
    *   statistics based on the move performed
    */
  def apply(searchResult: SearchResult, neighborhood: Neighborhood): NeighborhoodStats = {
    NeighborhoodStats(
      foundMove = searchResult match {
        case NoMoveFound  => false
        case MoveFound(_) => true
      },
      // In OscaR.CBLS lastCallGain is not set to zero if no move were found
      hasImproved = searchResult match {
        case NoMoveFound  => false
        case MoveFound(_) => NeighborhoodUtils.lastCallGain(neighborhood) > 0
      },
      slope = NeighborhoodUtils.slope(neighborhood),
      timeNano = NeighborhoodUtils.lastCallDuration(neighborhood)
    )
  }
}
