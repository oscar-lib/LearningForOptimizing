package logger

import combinator.{BanditSelector, NeighborhoodUtils}
import oscar.cbls.core.search
import oscar.cbls.core.search.{MoveFound, Neighborhood, SearchResult}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Logs information about the neighbor selection, so that it can be easily shown afterwards
 */
class NeighborSelectionHistory(val neighborhoods: List[Neighborhood]) {

  private var iteration: Long = 0

  // for each neighborhood, its index in the list
  protected var neighborhoodIdx: mutable.HashMap[Neighborhood, Int] = mutable.HashMap(neighborhoods.zipWithIndex: _*)

  // history of each neighborhood
  private val timeStamps: ArrayBuffer[TimeStamp] = new ArrayBuffer[TimeStamp]()

  // time at which a reset was performed
  private val resetTimeStamp: ArrayBuffer[TimedIteration] = new ArrayBuffer[TimedIteration]()

  private var lastResetCorrectlySet: Boolean = true;
  private val startTime = System.nanoTime();

  def notifySearchResult(neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    val currentTime = System.nanoTime();
    val elapsed = currentTime - startTime;
    val iteration = timeStamps.size
    val id = neighborhoodIdx(neighborhood)
    val timeStamp = TimeStamp(neighborhood, id, searchResult, iteration, elapsed)
    if (!lastResetCorrectlySet) {
      // values for reset are updated this way because it does not seem to be profiling information associated to reset operations
      val lastResetTimeBefore = timeStamps.last.timeAfterNano
      val lastResetTimeAfter = timeStamp.timeAfterNano;
      val resetTimedIteration = new TimedIteration(iteration, lastResetTimeBefore, lastResetTimeAfter)
      resetTimeStamp.append(resetTimedIteration)
      lastResetCorrectlySet = true
    }
    timeStamps.append(timeStamp)
  }

  def notifyReset(): Unit = {
    lastResetCorrectlySet = false;
  }

  /**
   * Gives a very large string describing the observed history
   * - first line: list of names for each neighborhood
   * - second line: list of timestamps
   * - third line: lis of resets
   * @return
   */
  override def toString: String = {
    s"neighborhoods=$neighborhoods\ntimestamps=${prettyStringArrayBufferGeneric(timeStamps)}\nresets=$resetTimeStamp"
  }

  def prettyStringArrayBufferGeneric[G](arrayBuffer: ArrayBuffer[G]): String = {
    arrayBuffer.map { row =>
      row.toString
    }.mkString("[", ", ", "]")
  }

}

class BanditSelectionHistory(neighborhoods: List[Neighborhood]) extends NeighborSelectionHistory(neighborhoods) {

  private val weights : ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]()
  private val probability : ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]()

  def notifySearchResult(banditSelector: BanditSelector, neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    val weight = neighborhoods
      .map(n => banditSelector.weight(n))
      .toArray
    val probability = neighborhoods
      .map(n => banditSelector.probability(n))
      .toArray
    this.weights.append(weight)
    this.probability.append(probability)
    super.notifySearchResult(neighborhood, searchResult)
  }

  override def toString: String = {
    val superString = super.toString
    s"$superString\nweights=${prettyStringArrayBuffer(weights)}\nprob=${prettyStringArrayBuffer(probability)}"
  }

  private def prettyStringArrayBuffer(arrayBuffer: ArrayBuffer[Array[Double]]): String = {
    arrayBuffer.map { row =>
      row.map(value => f"$value%.3f").mkString("(", ", ", ")")
    }.mkString("[", ", ", "]")
  }

}

class TimedIteration(val iteration: Long, val timeBeforeNano: Long, val durationNano: Long) {

}

class TimeStamp(val neighborhoodId: Int, val searchResult: SearchResult, val iteration: Long, val timeAfterNano: Long, val durationNano: Long) {

  override def toString: String = {
    s"(n=${neighborhoodId}, ${searchResult.getClass.getSimpleName.replace("$", "")}, i=$iteration, t=$timeAfterNano, d=$durationNano)"
  }

}

object TimeStamp {
  // Factory method
  def apply(neighborhood: Neighborhood, neighborhoodId: Int, searchResult: SearchResult, iteration: Long, timeAfterNano: Long): TimeStamp = {
    val profiler = NeighborhoodUtils.getProfiler(neighborhood)
    val duration = profiler._lastCallDurationNano
    new TimeStamp(neighborhoodId, searchResult, iteration, timeAfterNano, duration)
  }
}
