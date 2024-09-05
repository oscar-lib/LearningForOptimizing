package csp

import oscar.cbls.CBLSIntVar
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, Move}

object OneCarMoveNeighborhood {

  /** Neighborhoods dedicated for the CSP that moves a car of the sequence to another place.
    *
    * Through this process, some car could be shifted forward or backward in the sequence.
    *
    * Example : [0,1,2,3,4,5,6,7,8,9] Moving 5 before 1 [0,5,1,2,3,4,6,7,8,9]
    *
    * 1,2,3 and 4 were shifted forward
    *
    * NOTE : Way faster with CBLSSeqVar but we don't have the time to change everything about CSP
    *
    * @param vars
    *   The sequence representing the CSP
    * @param carsIndexToMove
    *   a function returning the car we are allowed to move
    * @param relevantPredecessorsIndex
    *   a function returning the relevant new predecessors of the value
    * @param name
    *   The name of this Neighborhood (default class name)
    */
  def apply(
    vars: Array[CBLSIntVar],
    carsIndexToMove: () => Iterable[Int] = () => Iterable.empty[Int],
    relevantPredecessorsIndex: Long => Iterable[Int] = _ => Iterable.empty[Int],
    name: String = "OneCarMove"
  ): OneCarMoveNeighborhood = {
    new OneCarMoveNeighborhood(vars, carsIndexToMove, relevantPredecessorsIndex, name)
  }
}

/** Neighborhoods dedicated for the CSP that moves a car of the sequence to another place.
  *
  * Through this process, some car could be shifted forward or backward in the sequence.
  *
  * Example : [0,1,2,3,4,5,6,7,8,9] Moving 5 before 1 [0,5,1,2,3,4,6,7,8,9]
  *
  * 1,2,3 and 4 were shifted forward
  *
  * NOTE : Way faster with CBLSSeqVar but we don't have the time to change everything about CSP
  *
  * @param vars
  *   The sequence representing the CSP
  * @param carsIndexToMove
  *   a function returning the car we are allowed to move.
  * @param relevantPredecessorsIndex
  *   a function returning the relevant new predecessors of the value.
  * @param name
  *   The name of this Neighborhood (default class name)
  */
class OneCarMoveNeighborhood(
  vars: Array[CBLSIntVar],
  carsIndexToMove: () => Iterable[Int],
  relevantPredecessorsIndex: Long => Iterable[Int],
  name: String
) extends EasyNeighborhoodMultiLevel[OneCarMoveMove](name) {

  private var carPosition: Int       = -1
  private var newPosition: Int       = -1
  private var goingBackward: Boolean = false

  private def evalObjAndRollBack(): Long = {
    val objValue = obj.value
    if (goingBackward) {
      for (i <- newPosition until carPosition)
        vars(i) :=: vars(i + 1)
    } else {
      for (i <- newPosition until carPosition by -1)
        vars(i) :=: vars(i - 1)
    }
    objValue
  }

  override def exploreNeighborhood(initialObj: Long): Unit = {
    val carsIndexToMoveNow: Iterable[Int] =
      if (carsIndexToMove().isEmpty) vars.indices
      else carsIndexToMove()

    val (carsIndexToMoveIterator, notifyFound1) = First().toIterator(carsIndexToMoveNow)

    for (carIndex <- carsIndexToMoveIterator) {
      carPosition = carIndex
      val carValue = vars(carPosition).value
      val relevantPredecessorsIndexNow =
        if (relevantPredecessorsIndex(carValue).isEmpty) vars.indices
        else relevantPredecessorsIndex(carValue)

      val (relevantPredecessorsIndexIterator, notifyFound2) =
        First().toIterator(relevantPredecessorsIndexNow)

      for (newPredecessorIndex <- relevantPredecessorsIndexIterator) {
        goingBackward = newPredecessorIndex < carPosition
        newPosition = if (goingBackward) newPredecessorIndex + 1 else newPredecessorIndex

        if (goingBackward) {
          for (i <- carPosition until newPosition by -1)
            vars(i) :=: vars(i - 1)
        } else {
          for (i <- carPosition until newPosition)
            vars(i) :=: vars(i + 1)
        }

        if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
          notifyFound1()
          notifyFound2()
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj: Long): OneCarMoveMove = {
    OneCarMoveMove(vars, carPosition, newPosition, goingBackward, newObj)
  }
}

case class OneCarMoveMove(
  vars: Array[CBLSIntVar],
  carPosition: Int,
  newPosition: Int,
  goingBackward: Boolean,
  override val objAfter: Long,
  name: String = "CarMoveMove"
) extends Move(objAfter, name) {

  override def commit(): Unit = {
    if (goingBackward) {
      for (i <- carPosition until newPosition by -1)
        vars(i) :=: vars(i - 1)
    } else {
      for (i <- carPosition until newPosition)
        vars(i) :=: vars(i + 1)
    }
  }
}
