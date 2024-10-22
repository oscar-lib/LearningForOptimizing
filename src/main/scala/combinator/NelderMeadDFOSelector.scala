package combinator

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood

/** A selector that rewards teams of neighborhoods
 * Each iteration attempts to replace the worst team by a better one, following Nelder–Mead method
 * See https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method for more explanations on the method
  */
class NelderMeadDFOSelector(neighborhoods: List[Neighborhood], o: Objective) {

  private val startingPoint = o.model.solution()
  private val n                                = neighborhoods.size
  private val candidates: Array[Array[Double]] = initialCandidates()
  private val performances: Array[Option[Double]] =
    neighborhoods.map(_ => None).toArray // the lower the better

  // when true, some performances are not evaluated yet.
  // need to run a first time all configurations to evaluate them
  private var initializationPhase: Boolean = true;

  private val wReflection  = 1.0
  private val wExpansion   = 2.0
  private val wContraction = 0.5
  private val wShrink      = 0.5

  private val bestIdx  = 0
  private val worstIdx = n

  private def initialCandidates(): Array[Array[Double]] = {
    val c            = Array[Array[Double]](n + 1)
    val lowerWeight  = 1.0 / (2.0 * n)
    val higherWeight = 1.0 - lowerWeight
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

  /** Replace worst candidate by a suitable one, according to the Nelder–Mead method
    */
  private def replaceWorstCandidate(): Unit = {
    // update the ranking
    rankCandidates()
    val worst = candidates(worstIdx)
    // compute the centroid, used to deduce a suitable weight vector to replace the worst one
    val centroid = computeCentroidOmittingWorst()
    // compute the reflection point
    val reflected =
      Range(0, n).map(i => centroid(i) + wReflection * (centroid(i) - worst(i))).toArray
    val performanceBest        = performances(bestIdx).get
    val performanceSecondWorst = performances(n - 2).get
    val performanceReflected   = evaluate(reflected)
    if (performanceBest <= performanceReflected && performanceReflected < performanceSecondWorst) {
      // reflection point is better than the second worst, use it
      replaceWorstBy(reflected, performanceReflected)
    } else if (performanceReflected < performanceBest) { // reflection is best so far, attempt to go further in this direction
      val expanded =
        Range(0, n).map(i => centroid(i) + wExpansion * (reflected(i) - centroid(i))).toArray
      val performanceExpanded = evaluate(expanded)
      if (performanceExpanded < performanceReflected) { // expanded better than reflection
        replaceWorstBy(expanded, performanceExpanded)
      } else { // reflection is still better
        replaceWorstBy(reflected, performanceReflected)
      }
    } else { // certain that reflected is at least as bad as the second worst option
      val performanceWorst = performances.last
      if (performanceReflected < performanceWorst) {
        val contractionIn =
          Range(0, n).map(i => centroid(i) + wContraction * (reflected(i) - centroid(i))).toArray
        val performanceContractionIn = evaluate(contractionIn)
        if (performanceContractionIn < performanceReflected) {
          replaceWorstBy(contractionIn, performanceContractionIn)
        } else {
          shrink()
        }
      } else { // reflected is more bad than the worst option
        val contractionOut =
          Range(0, n).map(i => centroid(i) + wContraction * (centroid(i) - reflected(i))).toArray
        val performanceContractionOut = evaluate(performanceContractionOut)
        if (performanceContractionOut < performanceReflected) {
          replaceWorstBy(contractionOut, performanceContractionOut)
        } else {
          shrink()
        }
      }
    }
  }

  /** Replaces all candidates xi except the best one with xi <- best + wShrink * (xi - best)
    */
  private def shrink(): Unit = {
    val best = candidates(bestIdx)
    for (i <- 1 to n) { // change all candidates except the best one (at index 0)
      for (j <- 0 until n) // xi <- best + wShrink * (xi - best)
        candidates(i)(j) = best(j) + wShrink * (candidates(i)(j) - best(j))
    }
  }

  /** Sort the candidates and their performance vector according to the
    */
  private def rankCandidates(): Unit = {}

  private def replaceWorstBy(weight: Array[Double], performance: Double): Unit = {
    candidates(worstIdx) = weight
    performances(worstIdx) = Some(performance)
  }

  /** Computes the centroid of all candidates, except the worst one
    * @return centroid of all candidates, except the worst one
    */
  private def computeCentroidOmittingWorst(): Array[Double] = {
    val centroid = Array.fill(n)(0.0)
    for (i <- 0 until n) // for all candidates except worst
      for (j <- 0 until n)
        centroid(j) += candidates(i)(j) // add coordinate
    for (j <- 0 until n) // transform sum into mean along the axis
      centroid(j) = centroid(j) / n
    centroid
  }

  /** Evaluates a given vector weights for the neighborhoods
    * @param weights
    *   weights to set for the neighborhoods
    * @return
    */
  private def evaluate(weights: Array[Double]): Double = {
    // reset the model to the starting point
    restoreStartingPoint()
    // performs the search with the provided weights

    // returns the final objective
    null
  }

  private def restoreStartingPoint(): Unit = {
    startingPoint.restoreDecisionVariables()
  }

}
