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

import util.SolverInput

import oscar.cbls.core.search.Neighborhood

import scala.util.Random

class EpsilonGreedyBanditNew(l: List[Neighborhood], in: SolverInput)
    extends BanditSelector(
      l,
      learningScheme = AfterEveryMove,
      learningRate = in.learningRate,
      rewardModel = new OriginalRewardModel(
        wSol = in.moveFoundWeight,
        wEff = in.efficiencyWeight,
        wSlope = in.slopeWeight
      )
    ) {

  private var t: Int  = 0 // number of times the bandit was called to provide the next neighborhood
  private val epsilon = in.epsilon

  /** The method that provides a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def getNextNeighborhood: Option[Neighborhood] = {
    t += 1
    val epsilon_t: Double = epsilon * Math.sqrt(l.length.toDouble / t)
    val prob_t: Double    = Random.nextDouble()
    if (prob_t > epsilon_t) { // gives the best neighborhood
      getBestNeighborhood
    } else { // return based on the weights as probability
      getNeighborhoodWithProbability
    }
  }
}
