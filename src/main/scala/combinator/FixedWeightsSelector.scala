package combinator

import oscar.cbls.core.search.Neighborhood

/**
 * A bandit selector with fixed weights, and no update to the weights ever being performed
 * @param neighborhoods
 *   neighborhoods available for selection
 * @param weights weights to use for the neighborhoods
 */
class FixedWeightsSelector(neighborhoods: List[Neighborhood], weights: List[Double])
  extends BanditSelector(neighborhoods, AfterEveryMove, rewardModel = new SlopeReward()) {

  for (i <- neighborhoods.indices) {
    setWeight(i, weights(i)) // set to the fixed weight
  }

  /** The method that provides a neighborhood.
   *
   * @return
   * Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = getNeighborhoodWithProbability

  /**
   * Never updates the weights :-)
   */
  override def updateWeight(neighborhood: Neighborhood): Unit = {}

}
