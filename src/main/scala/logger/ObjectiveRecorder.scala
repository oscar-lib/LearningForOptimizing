package logger

import oscar.cbls.core.objective.Objective

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** Observer that keeps the best objective values found and the time at which they were found This
  * class should be notified that the objective may have changed by calling notifyMove() after every
  * move
  * @param objective
  *   objective to monitor
  * @param getRealObjective
  *   function taking as input an oscar objective, and returning the objective of the real problem,
  *   or None if the oscar objective does not have a real objective yet
  */
class ObjectiveRecorder(objective: Objective, getRealObjective: Function[Long, Option[Double]]) {

  private var previousBestValue = objective.value // last best value observed
  private val initTimeNano =
    System.nanoTime() // time at which the object was created, used to compute the elapsed time
  // time is recorded in seconds, as a Double (more precision should not be necessary for plots)

  // objective improvements over time, where the objective is the one described in the oscar model
  // only absolute improvements are recorded (so the absolute best found solution)
  private val oscarObjectiveTimeStamp: ArrayBuffer[(Double, Long)] =
    new ArrayBuffer[(Double, Long)]()

  // objective improvements over time, where the objective is the real one of the problem
  // only absolute improvements are recorded (so the absolute best found solution)
  private val realObjectiveTimeStamp: ArrayBuffer[(Double, Double)] =
    new ArrayBuffer[(Double, Double)]()

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
      oscarObjectiveTimeStamp.append((elapsedTimeSeconds, currentValue))
      val realObjective = getRealObjective(currentValue)
      realObjective match {
        case Some(value) => realObjectiveTimeStamp.append((elapsedTimeSeconds, value))
        case None        =>
      }
    }
  }

  /** Returns the integral over time of the primal gap compared to a best known solution. The primal
    * gap is a value defined between 0 and 1. 1 Means no solution found, and 0 the best solution
    * found. The closer to 0, the better. The integral primal gap is the integral of the primal gap
    * over the time
    * @param bestKnownSolution
    *   best known solution used to compute the integral primal gap
    * @return
    *   integral over time of the primal gap
    */
  def integralPrimalGap(bestKnownSolution: Double, until: Double): Double = {
    val gapsOverTime = primalGapOverTime(bestKnownSolution, until)

    // computes the integral over time
    // iterate over the (time, gap) and compute the area of the rectangle
    // beginning at the previous time, ending at the current time, and having of a height of the previous observed gap
    val (integral, previousTime, previousGap) = gapsOverTime.foldLeft((0.0, 0.0, 1.0)) {
      case ((accIntegral, lastTime, lastGap), (currentTime, currentGap)) =>
        val rectangleArea = lastGap * (currentTime - lastTime)
        (accIntegral + rectangleArea, currentTime, currentGap)
    }

    // accounts for the time from the last recorded time to the 'until' time
    val lastRectangle = previousGap * (until - previousTime)
    integral + lastRectangle
  }

  /** Returns the primal gap over time: a collection of tuples of (time, gap). time is when a new
    * gap has been found, gap is the value of the gap at this point (defined in [0, 1])
    * @param bestKnownSolution
    *   reference solution used to compute the gap
    * @param until
    *   maximum time used to compute the gap
    * @return
    *   collection of (time, gap)
    */
  def primalGapOverTime(bestKnownSolution: Double, until: Double): ArrayBuffer[(Double, Double)] = {
    realObjectiveTimeStamp
      .takeWhile { case (time, _) => time <= until }
      .map { case (time, v) => (time, primalGap(bestKnownSolution, Some(v))) }
  }

  /** Computes the primal gap compared to a best known solution. The primal gap is a value defined
    * between 0 and 1. 1 Means no solution found, and 0 the best solution found. The closer to 0,
    * the better.
    * @param bestKnownSolution
    *   best known solution used to compute the integral primal gap
    * @param value
    *   value of the current solution, which should always be above the best known solution
    * @return
    */
  def primalGap(bestKnownSolution: Double, value: Option[Double]): Double = {
    value match {
      case Some(value) =>
        if (value == bestKnownSolution)
          0.0
        else
          Math.abs(bestKnownSolution - value) / Math
            .max(Math.abs(bestKnownSolution), Math.abs(value))
      case None => 1.0 // no solution found
    }
  }

  /** Retrieves the best known solution to an instance, according to a file where the best known
    * solution are written
    * @param filename
    *   path to the file where the best known solution are written
    * @param instanceName
    *   name of the instance
    * @return
    *   best known solution of the instance, if the instance was an entry of the file
    */
  def getBestKnownSolution(filename: String, instanceName: String): Option[Double] = {
    val content = Source.fromFile(filename)
    try {
      for (line <- content.getLines()) {
        val columns = line.split(",")
        // Check if the line has exactly two columns (instance, bks)
        if (columns.length == 2) {
          val instance = columns(0).trim
          if (instance == instanceName) {
            val bks = columns(1).trim
            return Some(bks.toDouble)
          }
        }
      }
    } catch {
      case _: Exception => return None
    } finally {
      content.close()
    }
    None
  }

  override def toString: String = {
    val timeStampString = oscarObjectiveTimeStamp
      .map(e => f"(t=${e._1}%.3f-v=${e._2})")
      .mkString("[", "-", "]")
    s"obj=$timeStampString"
  }

}
