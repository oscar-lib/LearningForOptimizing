package combinator

import oscar.cbls.core.search.Neighborhood

class UCBNew(neighborhoods: List[Neighborhood]) extends BanditSelector(neighborhoods, AfterEveryMove) {

  private var t: Int = 0 // number of times the bandit was called to provide the next neighborhood
  private val wSol = 0.4 // weight rewarding a move being found
  private val wEff = 0.2 // weight rewarding small execution time
  private val wSlope = 0.4 // weight rewarding the slope
  private var neigh_idx_max: Vector[Int] = Vector.empty

  /** The method that provides a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = {
    t += 1
    var maxUcb: Double = Double.MinValue
    neigh_idx_max = Vector.empty

    authorizedNeighorhoodIterator().foreach(idx => {
      val ucbIdx = if (nSelected(idx) == 0) Double.MinValue else
        weights(idx) / nSelected(idx) + math.sqrt(2 * math.log(t) / nSelected(idx))
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

  /** The method that computes a reward associated to a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def reward(runStat: NeighorhoodStats, neighborhood: Neighborhood): Double = {
    wSol * rewardFoundMove(runStat) +
      wEff * rewardExecutionTime(runStat) +
      wSlope * rewardSlope(runStat)
  }
}
