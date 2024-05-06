package combinator

import oscar.cbls.core.search.{Neighborhood, SearchResult}

import scala.util.Random

class EpsilonGreedyBanditNew(l: List[Neighborhood], epsilon: Double = 0.7)
  extends BanditSelector(l, learningScheme = AfterEveryMove) {

  private var t: Int = 0 // number of times the bandit was called to provide the next neighborhood
  private val wSol = 0.4 // weight rewarding a move being found
  private val wEff = 0.2 // weight rewarding small execution time
  private val wSlope = 0.4 // weight rewarding the slope

  /** The method that provides a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = {
    t += 1
    val epsilon_t: Double = epsilon * Math.sqrt(l.length.toDouble / t)
    val proba_t: Double = Random.nextDouble()
    if (proba_t > epsilon_t) { // gives the best neighborhood
        getBestNeighborhood()
    } else { // return based on the weights as probability
        getNeighborhoodWithProba()
    }
  }

  override def reward(runStat: NeighorhoodStats, neighborhood: Neighborhood): Double = {
    wSol * rewardFoundMove(runStat) +
      wEff * rewardExecutionTime(runStat) +
      wSlope * rewardSlope(runStat)
  }
}
