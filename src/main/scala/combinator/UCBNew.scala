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

class UCBNew(neighborhoods: List[Neighborhood], in: SolverInput)
    extends BanditSelector(
      neighborhoods,
      AfterEveryMove,
      learningRate = in.learningRate,
      rewardModel = new OriginalRewardModel()
    ) {

  private var t: Int = 0 // number of times the bandit was called to provide the next neighborhood
  private val wSol   = in.moveFoundWeight  // weight rewarding a move being found
  private val wEff   = in.efficiencyWeight // weight rewarding small execution time
  private val wSlope = in.slopeWeight      // weight rewarding the slope
  private val wConf  = in.confidence       // weight of the confidence width
  private var neigh_idx_max: Vector[Int] = Vector.empty

  /** The method that provides a neighborhood.
    *
    * @return
    *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  override def getNextNeighborhood: Option[Neighborhood] = {
    t += 1
    var maxUcb: Double = Double.MinValue
    neigh_idx_max = Vector.empty

    authorizedNeighborhoodIterator().foreach(idx => {
      val ucbIdx =
        if (nSelected(idx) == 0) Double.MinValue
        else
          weights(idx) / nSelected(idx) + wConf * math.sqrt(2 * math.log(t) / nSelected(idx))
      if (ucbIdx == maxUcb) {
        neigh_idx_max +:= idx
      } else if (ucbIdx > maxUcb) {
        maxUcb = ucbIdx
        neigh_idx_max = Vector(idx)
      }
    })

    val neigh_idx = neigh_idx_max(rand.nextInt(neigh_idx_max.size))
    Some(neighborhoods(neigh_idx))
  }

}
