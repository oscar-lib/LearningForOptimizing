// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package combinator

import oscar.cbls.core.search.Neighborhood

class BestSlopeFirstNew(neighborhoods: List[Neighborhood])
    extends BanditSelector(neighborhoods, AfterEveryMove) {

  /** The method that provides a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def getNextNeighborhood: Option[Neighborhood] = getBestNeighborhood

  /** The method that computes a reward associated to a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def reward(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
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
    *   new weight to set for the neighborhood
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
