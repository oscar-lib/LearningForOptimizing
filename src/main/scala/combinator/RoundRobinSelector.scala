package combinator

import oscar.cbls.core.search.Neighborhood

class RoundRobinSelector(neighborhoods: List[Neighborhood])
  extends BanditSelector(neighborhoods, learningScheme = Never, rewardModel = new SlopeReward()) {

  private var currentIdx = 0

  /** The method that provides a neighborhood.
   *
   * @return
   *   Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = {
    var idx = currentIdx
    var tries = 0
    while (isTabu(idx) && tries < neighborhoods.length) {
      tries += 1
      idx = idx + 1
      if (idx == neighborhoods.length)
        idx = 0
    }
    currentIdx = idx + 1
    if (currentIdx == neighborhoods.length)
      currentIdx = 0
    if (tries >= neighborhoods.length)
      None
    else
      Some(neighborhoods(idx))
  }

}
