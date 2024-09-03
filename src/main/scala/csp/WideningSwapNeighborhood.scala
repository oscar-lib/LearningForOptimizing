package csp

import oscar.cbls.CBLSIntVar
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First, Move}

object WideningSwapNeighborhood {

  /** Returns a Neighborhood dedicated for the CSP that swaps two sub-sequences of car.
    *
    * The minimum size of those sub-sequences should be at least 2 and the maximum at most sequence
    * size divided by 2. The swapped sub-sequences are of the same size.
    *
    * Example : [0,1,2,3,4,5,6,7,8,9] Swapping [2,3] and [6,7] [0,1,6,7,4,2,3,8,9]
    *
    * 1,2,3 and 4 were shifted forward
    *
    * NOTE : Way faster with CBLSSeqVar but we don't have the time to change everything about CSP
    *
    * @param vars
    *   The sequence representing the CSP
    * @param minSubSequenceSize
    *   The minimum size of the swapped sub-sequence (default 2)
    * @param maxSubSequenceSize
    *   The maximum size of the swapped sub-sequence (default 5)
    * @param searchZone1
    *   A function returning the car indices that are considered to start the first sub-sequence.
    * @param searchZone2
    *   A function returning the car indices that are considered to start the second sub-sequence
    *   based on the first segment start.
    * @param name
    *   The name of this Neighborhood (default class name)
    */
  def apply(
    vars: Array[CBLSIntVar],
    minSubSequenceSize: Int = 2,
    maxSubSequenceSize: Int = 5,
    searchZone1: () => Iterable[Int] = () => Iterable.empty[Int],
    searchZone2: Int => Iterable[Int] = _ => Iterable.empty[Int],
    name: String = "WideningSwapNeighborhood"
  ): WideningSwapNeighborhood = {
    new WideningSwapNeighborhood(
      vars,
      name,
      minSubSequenceSize,
      maxSubSequenceSize,
      searchZone1,
      searchZone2
    )
  }
}

/** Returns a Neighborhood dedicated for the CSP that swaps two sub-sequences of car.
  *
  * The minimum size of those sub-sequences should be at least 2 and the maximum at most sequence
  * size divided by 2. The swapped sub-sequences are of the same size.
  *
  * Example : [0,1,2,3,4,5,6,7,8,9] Swapping [2,3] and [6,7] [0,1,6,7,4,2,3,8,9]
  *
  * 1,2,3 and 4 were shifted forward
  *
  * NOTE : Way faster with CBLSSeqVar but we don't have the time to change everything about CSP
  *
  * @param vars
  *   The sequence representing the CSP
  * @param minSubSequenceSize
  *   The minimum size of the swapped sub-sequence (default 2)
  * @param maxSubSequenceSize
  *   The maximum size of the swapped sub-sequence (default 5)
  * @param searchZone1
  *   A function returning the car indices that are considered to start the first sub-sequence.
  * @param searchZone2
  *   A function returning the car indices that are considered to start the second sub-sequence
  *   based on the first segment start.
  * @param name
  *   The name of this Neighborhood (default class name)
  */
class WideningSwapNeighborhood(
  vars: Array[CBLSIntVar],
  name: String,
  minSubSequenceSize: Int,
  maxSubSequenceSize: Int,
  searchZone1: () => Iterable[Int],
  searchZone2: Int => Iterable[Int]
) extends EasyNeighborhoodMultiLevel[WideningSwapMove](name) {

  private var from_1, from_2, to_1, to_2 = -1

  private def evalObjAndRollBack(): Long = {
    val objValue = obj.value
    for (i <- 0 until to_1 - from_1) {
      vars(from_1 + i) :=: vars(from_2 + i)
    }
    objValue
  }

  override def exploreNeighborhood(initialObj: Long): Unit = {
    val maxSize: Int = vars.length - (vars.length % maxSubSequenceSize)

    val searchZone1Now: Iterable[Int] =
      if (searchZone1().isEmpty) 0 until maxSize - maxSubSequenceSize by maxSubSequenceSize
      else searchZone1()

    val (startPos1Iterator, notifyFound1) = First().toIterator(searchZone1Now)

    for (startPos_1 <- startPos1Iterator) {
      from_1 = startPos_1
      val searchZone2Now =
        if (searchZone2(from_1).isEmpty) maxSubSequenceSize until maxSize by maxSubSequenceSize
        else searchZone2(from_1)
      val (startPos2Iterator, notifyFound2) = First().toIterator(searchZone2Now)

      for (startPos_2 <- startPos2Iterator) {
        from_2 = startPos_2
        for (segmentSize <- minSubSequenceSize to maxSubSequenceSize) {
          to_1 = from_1 + segmentSize
          to_2 = from_2 + segmentSize

          for (i <- 0 until to_1 - from_1)
            vars(from_1 + i) :=: vars(from_2 + i)
          if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
            notifyFound1()
            notifyFound2()
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj: Long): WideningSwapMove = {
    WideningSwapMove(vars, from_1, to_1, from_2, to_2, name, newObj)
  }
}

case class WideningSwapMove(
  vars: Array[CBLSIntVar],
  from_1: Int,
  to_1: Int,
  from_2: Int,
  to_2: Int,
  name: String,
  override val objAfter: Long
) extends Move(objAfter, name) {

  require(to_1 - from_1 == to_2 - from_2, "Swapped segment must have the same size")

  override def commit(): Unit = {
    for (i <- 0 until to_1 - from_1)
      vars(from_1 + i) :=: vars(from_2 + i)
  }
}
