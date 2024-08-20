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

package csp

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.neighborhoods.{SwapsNeighborhood, WideningFlipNeighborhood}
import oscar.cbls.modeling.StandardNeighborhoods

case class SimpleNeighborhoods(oscarModel: Model) extends StandardNeighborhoods {

  private val carSeq = oscarModel.carSequence

  def swaps(): SwapsNeighborhood = swapsNeighborhood(
    carSeq,
    "mostViolatedSwap",
    searchZone2 = () => { val v = oscarModel.mostViolatedCars.value; (_, _) => v },
    symmetryCanBeBrokenOnIndices = false
  )

  def shuffle(maxMovesNum: Int): Neighborhood =
    shuffleNeighborhood(carSeq, name = "shuffleAllCars") maxMoves maxMovesNum

  def wideningFlip(): WideningFlipNeighborhood = WideningFlipNeighborhood(carSeq)

  // todo: add other standard neighs.?
  // todo: implement subsequence swap?
}
