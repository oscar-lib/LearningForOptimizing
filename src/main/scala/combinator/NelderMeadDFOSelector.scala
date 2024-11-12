package combinator

import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.distributed.{IndependentMove, IndependentSolution}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{AcceptanceCriterion, IndependentLoadSolutionMove, LoadSolutionMove, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}
import oscar.cbls.lib.search.combinators.{BasicSaveBest, RestoreBestOnExhaust}

/** A selector that rewards teams of neighborhoods Each iteration attempts to replace the worst team
  * by a better one, following Nelder–Mead method See
  * https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method for more explanations on the method
  */
class NelderMeadDFOSelector(
  neighborhoods: List[Neighborhood],
  o: Objective,
  iterPerNeighborhood: Int = 5
) extends NeighborhoodCombinator(neighborhoods: _*) {

  private val startingPoint                           = o.model.solution()
  private val startingPointObjective = o.value
  private val n                                       = neighborhoods.size
  private val candidatesWeights: Array[Array[Double]] = initialCandidates()
  private val candidatesPerformances: Array[Option[Long]] =
    neighborhoods.map(_ => None).toArray // the lower the better
  private val ranking = (0 to n).toArray

  private var currentCandidateIdx = 0
  private var currentCandidateWeight: Array[Double] = candidatesWeights(
    currentCandidateIdx
  ) // weight of the current candidate being evaluated
  private val currentCandidate: FixedWeightsSelector = new FixedWeightsSelector(
    neighborhoods,
    currentCandidateWeight
  ) // current candidate being evaluated
  private var currentPerformance: Long =
    Long.MaxValue // performance of the current candidate being evaluated

  private var centroid: Array[Double]  = null
  private var reflectedPerformance     = Long.MaxValue
  private var reflected: Array[Double] = null

  // phases used within the algorithm
  private final val Initialization = 0 // evaluating the first set of candidates
  private final val Reflection     = 1 // computing the evaluation for the reflection
  private final val Expansion      = 2 // computing the evaluation for the expansion
  private final val ContractionIn  = 3 // computing the evaluation for the contraction in
  private final val ContractionOut = 4 // computing the evaluation for the contraction out
  private final val Shrink         = 5 // computing the evaluation for the shrink
  private var currentPhase = Initialization // current evaluation phase, one of the above
  private var t            = 0              // how many getMoves are done on the current candidate

  // weights for each phase
  private val wReflection  = 1.0
  private val wExpansion   = 2.0
  private val wContraction = 0.5
  private val wShrink      = 0.5

  // constant for easier reading of best, worst and second worst values
  private val bestIdx        = 0
  private val worstIdx       = n
  private val secondWorstIdx = n - 1

  private def initialCandidates(): Array[Array[Double]] = {
    val c: Array[Array[Double]] = Array.fill(n + 1, n)(0.0)
    val lowerWeight             = 1.0 / (2.0 * n)
    val higherWeight            = 1.0 - lowerWeight
    // n weights biased toward one of the neighborhood
    for (i <- 0 until n) {
      val weight = Array.fill(n)(lowerWeight)
      weight(i) = higherWeight
      c(i) = weight
    }
    // same probability of choosing any neighborhood
    c(n) = Array.fill(n)(1.0 / n)
    c
  }

  /** Replaces all candidates xi except the best one with xi <- best + wShrink * (xi - best)
    */
  private def shrink(): Unit = {
    currentPhase = Shrink
    val best = candidatesWeights(bestIdx)
    for (i <- 1 to n) { // change all candidates except the best one (at index 0)
      for (j <- 0 until n) // xi <- best + wShrink * (xi - best)
        candidatesWeights(i)(j) = best(j) + wShrink * (candidatesWeights(i)(j) - best(j))
    }
    currentCandidateIdx = 0 // tells that the candidates must all be evaluated
    replaceCurrentBy(candidatesWeights(currentCandidateIdx))
  }

  /** Updates the ranking array based on the recorded performances
    */
  private def rankCandidates(): Unit = {
    // Sort `ranking` in place based on the values in `performances`
    val sortedRanking = ranking.sortBy(index => candidatesPerformances(index).get)
    Array.copy(sortedRanking, 0, ranking, 0, ranking.length)
  }

  private def replaceWorstBy(weight: Array[Double], performance: Long): Unit = {
    candidatesWeights(worstIdx) = weight
    candidatesPerformances(worstIdx) = Some(performance)
  }

  /** Computes the centroid of all candidates, except the worst one
    * @return
    *   centroid of all candidates, except the worst one
    */
  private def computeCentroidOmittingWorst(): Array[Double] = {
    val centroid = Array.fill(n)(0.0)
    for (i <- ranking.tail.reverse) // for all candidates except worst
      for (j <- 0 until n) // for all coordinates
        centroid(j) += candidatesWeights(i)(j) // add coordinate
    for (j <- 0 until n) // transform sum into mean along the axis
      centroid(j) = centroid(j) / n
    centroid
  }

  /** Updates the list of candidates
    */
  private def updateCandidate(): Unit = {
    val performance = getPerformance()
    if (currentPhase == Initialization || currentPhase == Shrink) {
      candidatesPerformances(currentCandidateIdx) = Some(performance)
      currentCandidateIdx += 1
      if (currentCandidateIdx == n) {
        currentPhase = Reflection // all candidates were evaluated, switches to reflection phase
        updateCentroidAndReflected()
        replaceCurrentBy(reflected)
      } else { // continue evaluating an unranked candidate
        replaceCurrentBy(candidatesWeights(currentCandidateIdx))
      }
    } else if (currentPhase == Reflection) {
      // reflection ended
      reflectedPerformance = performance // updates the reflection performance
      val performanceBest        = candidatesPerformances(ranking(bestIdx)).get
      val performanceSecondWorst = candidatesPerformances(ranking(secondWorstIdx)).get
      if (
        performanceBest <= reflectedPerformance && reflectedPerformance < performanceSecondWorst
      ) {
        // reflection point is better than the second worst, use it
        replaceWorstBy(reflected, reflectedPerformance)
        // next call will also be a reflection. Updates the ranking, centroid and reflected point
        updateCentroidAndReflected()
        replaceCurrentBy(reflected)
      } else if (reflectedPerformance < performanceBest) {
        // reflection is best so far, attempt to go further in this direction
        currentPhase = Expansion
        val expanded =
          Range(0, n).map(i => centroid(i) + wExpansion * (reflected(i) - centroid(i))).toArray
        replaceCurrentBy(expanded)
      } else {
        // certain that reflected is at least as bad as the second worst option
        val worstPerformance = candidatesPerformances(ranking(worstIdx)).get
        if (reflectedPerformance < worstPerformance) {
          currentPhase = ContractionIn
          val contractionIn =
            Range(0, n).map(i => centroid(i) + wContraction * (reflected(i) - centroid(i))).toArray
          replaceCurrentBy(contractionIn)
        } else {
          currentPhase = ContractionOut
          val contractionOut =
            Range(0, n).map(i => centroid(i) + wContraction * (centroid(i) - reflected(i))).toArray
          replaceCurrentBy(contractionOut)
        }
      }
    } else {
      currentPhase match {
        case Expansion =>
          if (currentPerformance < reflectedPerformance) { // replace by current candidate (expansion)
            replaceWorstBy(currentCandidateWeight, currentPerformance)
          } else { // use the reflection instead
            replaceWorstBy(reflected, reflectedPerformance)
          } // next iteration is a reflection
          updateCentroidAndReflected()
          replaceCurrentBy(reflected)
        case ContractionIn | ContractionOut =>
          if (currentPerformance < reflectedPerformance) { // replace by current candidate (contraction)
            replaceWorstBy(currentCandidateWeight, currentPerformance)
            // next iteration is a reflection
            updateCentroidAndReflected()
            replaceCurrentBy(reflected)
          } else {
            shrink()
          }
      }
    }
  }

  private def updateCentroidAndReflected(): Unit = {
    rankCandidates()
    val worst = candidatesWeights(ranking(worstIdx))
    centroid = computeCentroidOmittingWorst()
    reflected = Range(0, n).map(i => centroid(i) + wReflection * (centroid(i) - worst(i))).toArray
  }

  private def replaceCurrentBy(weight: Array[Double]): Unit = {
    currentCandidateWeight = weight
    currentCandidate.enforceWeightVector(currentCandidateWeight)
  }

  private def currentEvaluationFinished(): Boolean = {
    t > iterPerNeighborhood
  }

  private def getCurrentCandidate(): Neighborhood = {
    currentCandidate
  }

  private def getPerformance(): Long = {
    currentPerformance
  }

  private def updateCurrentCandidateObjective(value: Long) = {
    currentPerformance = Math.min(value, currentPerformance)
  }

  private def resetForNewCandidate(): SearchResult = {
    t = 0 // restore timestamp
    currentPerformance = Long.MaxValue
    MoveFound(LoadSolutionMove(startingPoint, startingPointObjective, "DFO reset"))
  }

  private def notifyGetMove(): Unit = {
    t = t + 1
  }

  override def getMove(
    obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion
  ): SearchResult = {
    val v = obj.value
    println("t = " + t + " - phase = " + currentPhase + " idx = " + currentCandidateIdx)
    notifyGetMove()
    println(o.model.solution())
    if (currentEvaluationFinished()) { // evaluation phase finished for the current candidate
      updateCandidate()                // update the candidates list
      resetForNewCandidate()           // update timestamps and restore to starting point
    } else {
      // updates the best objective found for the candidate
      val objValue = obj.value
      updateCurrentCandidateObjective(objValue)
      getCurrentCandidate().getMove(obj, initialObj, acceptanceCriterion)
    }
  }

}
