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

/** This class exposes several methods that should be helpful in declaring a series of neighborhoods
  * to use for solving the car sequencing problem with local search.
 *
  * @param cspModel the model representing the csp instance
  */
case class SimpleNeighborhoods(cspModel: Model) extends StandardNeighborhoods {

  private val carSeq = cspModel.carSequence

  def swap(): Neighborhood = swapsNeighborhood(
    carSeq,
    "swap everywhere",
    symmetryCanBeBrokenOnIndices = false,
    symmetryCanBeBrokenOnValue = true
  )

  def swapMostViolated(): Neighborhood = swapsNeighborhood(
    carSeq,
    "mostViolatedSwap",
    searchZone2 = () => { val v = cspModel.mostViolatedCars.value; (_, _) => v },
    symmetryCanBeBrokenOnIndices = false,
    symmetryCanBeBrokenOnValue = true
  )

  def shuffle(
    indices: Option[() => Iterable[Int]] = None,
    numOfPositions: Option[Int] = None
  ): Neighborhood = {
    ShuffleNeighborhood(
      carSeq,
      indices match {
        case Some(x) => x
        case None    => () => Iterable.empty
      },
      numberOfIdToShuffle = numOfPositions match {
        case Some(x) => x
        case None    => Int.MaxValue
      },
      name = "Shuffle Cars"
    )
  }

  def wideningFlip(): Neighborhood = WideningFlipNeighborhood(carSeq)

  // todo: add other standard neighs.?
  // todo: implement subsequence swap?
}
