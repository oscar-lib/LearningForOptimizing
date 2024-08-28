package logger

import oscar.cbls.core.objective.Objective

import scala.collection.mutable.ArrayBuffer

/** Observer that keeps the best objective values found and the time at which they were found This
  * class should be notified that the objective may have changed by calling notifyMove() after every
  * move
  * @param objective
  *   objective to monitor
  */
class ObjectiveRecorder(objective: Objective) {

  private var previousBestValue = objective.value // last best value observed
  private val initTimeNano =
    System.nanoTime() // time at which the object was created, used to compute the elapsed time
  // time is recorded in seconds, as a Double (more precision should not be necessary for plots)
  private val timeStamp: ArrayBuffer[(Double, Long)] = new ArrayBuffer[(Double, Long)]()

  /** Notifies that a move has been performed, possibly recording the best objective value if the
    * objective has been improved
    */
  def notifyMove(): Unit = {
    val currentValue = objective.value
    if (currentValue < previousBestValue) {
      previousBestValue = currentValue
      val currentTimeNano    = System.nanoTime()
      val elapsedTimeNano    = currentTimeNano - initTimeNano
      val elapsedTimeSeconds = elapsedTimeNano.toDouble / 1_000_000_000.0
      timeStamp.append((elapsedTimeSeconds, currentValue))
    }
  }

  override def toString: String = {
    val timeStampString = timeStamp
      .map(e => f"(t=${e._1}%.3f-v=${e._2})")
      .mkString("[", "-", "]")
    s"obj=$timeStampString"
  }

}
