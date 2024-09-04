package csp

import oscar.cbls.CBLSIntVar
import oscar.cbls.core.search.{EasyNeighborhood, Move}

import scala.util.Random

object ShuffleNeighborhood {

  /** Neighborhood that shuffles some values of the variables in the Array.
    *
    * This Neighborhood is very likely to worsen the solution so be sure to add an AcceptAll
    * combinator with it.
    *
    * ### EVERY DAY I AM SHUFFLING ###
    *
    * @param vars
    *   The array containing the CBLSIntVar whose values will be shuffled
    * @param carIndexToConsider
    *   A function returning the list of index of variable that the neighborhood is allowed to
    *   shuffle.
    * @param numberOfIdToShuffle
    *   The maximum amount of id to shuffle
    * @param name
    *   The name of the Neighborhood
    */
  def apply(
    vars: Array[CBLSIntVar],
    carIndexToConsider: () => Iterable[Int] = () => Iterable.empty,
    numberOfIdToShuffle: Int = Int.MaxValue,
    name: String = "ShuffleNeighborhood"
  ): ShuffleNeighborhood = {
    new ShuffleNeighborhood(vars, carIndexToConsider, numberOfIdToShuffle, name)
  }
}

/** Neighborhood that shuffles some values of the variables in the Array.
  *
  * This Neighborhood is very likely to worsen the solution so be sure to add an AcceptAll
  * combinator with it.
  *
  * @param vars
  *   The array containing the CBLSIntVar whose values will be shuffled
  * @param carIndexToConsider
  *   A function returning the list of index of variable that the neighborhood is allowed to
  *   shuffle.
  * @param numberOfIdToShuffle
  *   The maximum amount of id to shuffle
  * @param name
  *   The name of the Neighborhood
  */
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

/** A move that applies a Shuffle move
  *
  * @param vars
  *   The Array of variable containing the shuffled value
  * @param shuffledCarIndices
  *   The indices of the shuffled variables
  * @param shuffledCarNewValues
  *   The new values of the shuffled variables
  * @param objAfter
  *   The objective function after the shuffling
  * @param name
  *   The name of the move
  */
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
