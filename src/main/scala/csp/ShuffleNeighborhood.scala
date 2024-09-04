package csp

import oscar.cbls.CBLSIntVar
import oscar.cbls.core.search.{EasyNeighborhood, Move}

import scala.util.Random

object ShuffleNeighborhood {
  def apply(
    vars: Array[CBLSIntVar],
    carIndexToConsider: () => Iterable[Int] = () => Iterable.empty,
    numberOfIdToShuffle: Int = Int.MaxValue,
    name: String = "ShuffleNeighborhood"
  ): ShuffleNeighborhood = {
    new ShuffleNeighborhood(vars, carIndexToConsider, numberOfIdToShuffle, name)
  }
}

class ShuffleNeighborhood(
  vars: Array[CBLSIntVar],
  carIndexToConsider: () => Iterable[Int],
  numberOfIdToShuffle: Int,
  name: String
) extends EasyNeighborhood[ShuffleMove](neighborhoodName = name) {

  private var initValues: List[Long] = List.empty

  private def evaluateAndRollBack(): Long = {
    val objAfter = obj.value
    shuffledCarIndices.zip(shuffledCarInitialValues).foreach(iv => vars(iv._1) := iv._2)
    objAfter
  }

  private var shuffledCarIndices: Iterable[Int]        = Iterable.empty
  private var shuffledCarInitialValues: Iterable[Long] = Iterable.empty
  private var shuffledCarNewValues: Iterable[Long]     = Iterable.empty

  override def exploreNeighborhood(): Unit = {
    initValues = vars.map(_.value).toList

    val carIndexToConsiderNow: List[Int] =
      if (carIndexToConsider().isEmpty) vars.indices.toList else carIndexToConsider().toList

    shuffledCarIndices = Random
      .shuffle(carIndexToConsiderNow)
      .take(Math.min(carIndexToConsiderNow.size, numberOfIdToShuffle))
    shuffledCarInitialValues = shuffledCarIndices.map(i => vars(i).value)
    shuffledCarNewValues = Random.shuffle(shuffledCarInitialValues).toArray

    shuffledCarIndices.zip(shuffledCarNewValues).foreach(iv => vars(iv._1) := iv._2)
    evaluateCurrentMoveObjTrueIfStopRequired(evaluateAndRollBack())

  }

  override def instantiateCurrentMove(newObj: Long): ShuffleMove = {
    ShuffleMove(vars, shuffledCarIndices, shuffledCarNewValues, newObj, "ShuffleMove")
  }
}

case class ShuffleMove(
  vars: Array[CBLSIntVar],
  shuffledCarIndices: Iterable[Int],
  shuffledCarNewValues: Iterable[Long],
  override val objAfter: Long,
  name: String
) extends Move(objAfter, name) {

  override def commit(): Unit = {
    shuffledCarIndices.zip(shuffledCarNewValues).foreach(iv => vars(iv._1) := iv._2)
  }
}
