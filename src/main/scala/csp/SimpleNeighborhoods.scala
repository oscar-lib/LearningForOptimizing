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
  * @param cspModel
  *   the model representing the csp instance
  */
case class SimpleNeighborhoods(cspModel: Model) extends StandardNeighborhoods {

  private val carSeq = cspModel.carSequence

  /** Neighborhood that swaps cars in the sequence. */
  def swap(): Neighborhood = swapsNeighborhood(
    carSeq,
    "swap everywhere",
    symmetryCanBeBrokenOnIndices = false,
    symmetryCanBeBrokenOnValue = true
  )

  /** Neighborhood that swaps most violated cars in the sequence. */
  def swapMostViolated(): Neighborhood = swapsNeighborhood(
    carSeq,
    "mostViolatedSwap",
    searchZone2 = () => { val v = cspModel.mostViolatedCars.value; (_, _) => v },
    symmetryCanBeBrokenOnIndices = false,
    symmetryCanBeBrokenOnValue = true
  )

  /** Restart Neighborhood that shuffles the car in the sequence. */
  def shuffle(
    indices: Option[() => Iterable[Int]] = None,
    numOfPositions: Option[Int] = None
  ): Neighborhood = {
    shuffleNeighborhood(
      carSeq,
      indices match {
        case Some(x) => x
        case None    => null
      },
      numberOfShuffledPositions = numOfPositions match {
        case Some(x) => () => x
        case None    => () => Int.MaxValue
      },
      name = "Shuffle Cars"
    )
  }

  /** Neighborhood that flips a varying sized subsequence of the sequence. */
  def wideningFlip(): Neighborhood = WideningFlipNeighborhood(carSeq)

  /** Neighborhood that flips a varying sized subsequence of the sequence with a most violated car
    * at the center.
    */
  def wideningFlipMostViolated(): Neighborhood =
    WideningFlipNeighborhood(
      carSeq,
      allowedPositions = () => cspModel.violatedCars.value,
      name = "WideningFlipMostViolated"
    )

  /** Neighborhood that swaps subsequence of size varying from 2 up to max opt seq length */
  def wideningSwap(): Neighborhood =
    WideningSwapNeighborhood(carSeq, maxSubSequenceSize = cspModel.instance.optSeqLen.max)

  /** Neighborhood that swaps subsequence of size varying from 2 up to max opt seq length starting
    * at a most violated car
    */
  def wideningSwapMostViolated(): Neighborhood = {
    val maxSegmentSize = cspModel.instance.optSeqLen.max
    WideningSwapNeighborhood(
      carSeq,
      maxSubSequenceSize = maxSegmentSize,
      searchZone1 =
        () => cspModel.mostViolatedCars.value.filter(_ + maxSegmentSize < carSeq.length),
      searchZone2 = mv => {
        (mv - maxSegmentSize until 0 by -maxSegmentSize) ++
          (mv + maxSegmentSize until carSeq.length - maxSegmentSize by maxSegmentSize)
      },
      name = "WideningSwapMostViolated"
    )
  }

  /** Neighborhood that move one violated car to another spot in the sequence. Avoiding places right
    * next to same car type.
    */
  def oneCarMove(): Neighborhood = OneCarMoveNeighborhood(
    carSeq,
    carsIndexToMove = () => cspModel.violatedCars.value,
    relevantPredecessorsIndex = carValue =>
      carSeq.zipWithIndex.filter(valueAndIndex => valueAndIndex._1.value != carValue).map(_._2)
  )

  /** Neighborhood that move one of the most violated car to another spot in the sequence. Avoiding
    * places right next to same car type.
    */
  def oneCarMoveMostViolated(): Neighborhood = OneCarMoveNeighborhood(
    carSeq,
    carsIndexToMove = () => cspModel.mostViolatedCars.value,
    relevantPredecessorsIndex = carValue =>
      carSeq.zipWithIndex.filter(valueAndIndex => valueAndIndex._1.value != carValue).map(_._2),
    name = "OneCarMoveMostViolated"
  )
}
