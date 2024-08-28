package logger

import combinator.{BanditSelector, NeighborhoodUtils}
import oscar.cbls.core.search
import oscar.cbls.core.search.{MoveFound, Neighborhood, NeighborhoodCombinator, SearchResult}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Logs information about the neighbor selection, so that it can be easily shown afterwards
  */
class NeighborSelectionHistory(val neighborhoods: List[Neighborhood]) {

  private var iteration: Long = 0

  // for each neighborhood, its index in the list
  protected var neighborhoodIdx: mutable.HashMap[Neighborhood, Int] =
    mutable.HashMap(neighborhoods.zipWithIndex: _*)

  // history of each neighborhood
  private val timeStamps: ArrayBuffer[TimeStamp] = new ArrayBuffer[TimeStamp]()

  // time at which a reset was performed
  private val resetTimeStamp: ArrayBuffer[TimedIteration] = new ArrayBuffer[TimedIteration]()

  private var lastResetCorrectlySet: Boolean = true;
  private val startTime                      = System.nanoTime();

  def notifySearchResult(neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    val currentTime = System.nanoTime();
    val elapsed     = currentTime - startTime;
    val iteration   = timeStamps.size
    val id          = neighborhoodIdx(neighborhood)
    val timeStamp   = TimeStamp(neighborhood, id, searchResult, iteration, elapsed)
    if (!lastResetCorrectlySet) {
      // values for reset are updated this way because it does not seem to be profiling information associated to reset operations
      val lastResetTimeBefore = timeStamps.last.timeAfterNano
      val lastResetTimeAfter  = timeStamp.timeAfterNano;
      val resetTimedIteration =
        new TimedIteration(iteration, lastResetTimeBefore, lastResetTimeAfter)
      resetTimeStamp.append(resetTimedIteration)
      lastResetCorrectlySet = true
    }
    timeStamps.append(timeStamp)
  }

  def notifyReset(): Unit = {
    lastResetCorrectlySet = false;
  }

  /** Gives a very large string describing the observed history
    *   - first line: list of names for each neighborhood
    *   - second line: list of timestamps
    *   - third line: lis of resets
    * @return
    */
  override def toString: String = {
    s"neighborhoods=${prettyStringGeneric(neighborhoods)}\n" +
      s"timestamps=${prettyStringGeneric(timeStamps)}\n" +
      s"resets=${prettyStringGeneric(resetTimeStamp)}"
  }

  def prettyStringGeneric[G](iter: Iterable[G]): String = {
    iter
      .map { entry =>
        entry.toString
      }
      .mkString("[", ", ", "]")
  }

}

class BanditSelectionHistory(neighborhoods: List[Neighborhood])
    extends NeighborSelectionHistory(neighborhoods) {

  private val weights: ArrayBuffer[Array[Double]]     = new ArrayBuffer[Array[Double]]()
  private val probability: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]()
  private val rewards: ArrayBuffer[NeighborhoodEntry[Double]] =
    new ArrayBuffer[NeighborhoodEntry[Double]]()

  def notifySearchResult(
    banditSelector: BanditSelector,
    neighborhood: Neighborhood,
    searchResult: SearchResult
  ): Unit = {
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

  def notifyReward(neighborhood: Neighborhood, reward: Double): Unit = {
    val neighborhoodId = neighborhoodIdx(neighborhood)
    rewards.append(new NeighborhoodEntry[Double](neighborhoodId, reward))
  }

  override def toString: String = {
    val superString = super.toString
    s"$superString\n" +
      s"weights=${prettyStringArrayBuffer(weights)}\n" +
      s"prob=${prettyStringArrayBuffer(probability)}\n" +
      s"reward=${prettyStringGeneric(rewards)}"
  }

  private def prettyStringArrayBuffer(arrayBuffer: ArrayBuffer[Array[Double]]): String = {
    arrayBuffer
      .map { row =>
        row.map(value => f"$value%.3f").mkString("(", ", ", ")")
      }
      .mkString("[", ", ", "]")
  }

}

class NeighborhoodEntry[G](val neighborhood: Int, val entry: G) {

  override def toString: String = {
    s"(n=$neighborhood, v=${entry match {
        case e: Double => f"$e%.3f"
        case _         => f"$entry"
      }})"
  }

}

object BanditSelectionHistory {

  /** Optionally retrieves the string representing the history of the moves being formed For this to
    * work, the neighborhood must be a BanditSelector, or contain a BanditSelector
    *
    * @param neighborhood
    *   neighborhood whose history must be retrieved as a String
    * @return
    *   String representation of the history, provided that the neighborhood contained a
    *   BanditSelector
    */
  def historyString(neighborhood: Neighborhood): Option[String] = {
    neighborhood match {
      case bandit: BanditSelector =>
        Some(bandit.history.toString)
      case n: NeighborhoodCombinator =>
        for (sub <- n.subNeighborhoods) {
          // recursive call to get the history of the sub-neighborhood contained in this neighborhood
          val substring = historyString(sub)
          substring match {
            case Some(value) => return Some(value) // return first valid description being found
            case None        =>
          }
        }
        None
      case _ =>
        None
    }
  }

}

class TimedIteration(val iteration: Long, val timeBeforeNano: Long, val durationNano: Long) {

  override def toString: String = {
    s"(i=$iteration, t=$timeBeforeNano, d=$durationNano)"
  }

}

class TimeStamp(
  val neighborhoodId: Int,
  val searchResult: SearchResult,
  val iteration: Long,
  val timeAfterNano: Long,
  val durationNano: Long
) {

  override def toString: String = {
    s"(n=${neighborhoodId}, ${searchResult.getClass.getSimpleName
        .replace("$", "")}, i=$iteration, t=$timeAfterNano, d=$durationNano)"
  }

}

object TimeStamp {
  // Factory method
  def apply(
    neighborhood: Neighborhood,
    neighborhoodId: Int,
    searchResult: SearchResult,
    iteration: Long,
    timeAfterNano: Long
  ): TimeStamp = {
    val profiler = NeighborhoodUtils.getProfiler(neighborhood)
    val duration = profiler._lastCallDurationNano
    new TimeStamp(neighborhoodId, searchResult, iteration, timeAfterNano, duration)
  }
}
