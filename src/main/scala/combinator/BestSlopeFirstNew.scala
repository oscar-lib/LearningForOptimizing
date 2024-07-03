package combinator

import oscar.cbls.core.search.Neighborhood

class BestSlopeFirstNew(neighborhoods: List[Neighborhood])
    extends BanditSelector(neighborhoods, AfterEveryMove) {

  /** The method that provides a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def getNextNeighborhood: Option[Neighborhood] = {
    getBestNeighborhood()
  }

  /** The method that computes a reward associated to a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def reward(runStat: NeighorhoodStats, neighborhood: Neighborhood): Double = {
    rewardSlope(runStat)
  }

  /** Use the reward to setup the new weight
    * @param neighborhood
    *   neighborhood on which the reward has been computed
    * @param oldWeight
    *   old weight of the neighborhood
    * @param reward
    *   reward associated to the neighborhood
    * @return
    *   new weight to set for the neigborhood
    */
  override def newWeightFromReward(
    neighborhood: Neighborhood,
    oldWeight: Double,
    reward: Double
  ): Double = {
    // the new weight is entirely composed of the slope in this case
    // there is no link with the previous weight
    reward
  }
}
