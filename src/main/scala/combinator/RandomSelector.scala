package combinator

import oscar.cbls.core.search.Neighborhood

class RandomSelector(neighborhoods: List[Neighborhood])
  extends BanditSelector(neighborhoods, learningScheme = Never, rewardModel = new SlopeReward()) {

  /** The method that provides a neighborhood.
   *
   * @return
   *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = getRandomNeighborhood

}
