package combinator

import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

/**
 * Lightweight store of information regarding the execution of a move
 *
 * @param hasImproved
 * @param foundMove
 * @param slope
 * @param timeNano
 */
case class NeighorhoodStats(
                             hasImproved: Boolean,
                             foundMove: Boolean,
                             slope: Double,
                             timeNano: Long,
                  )

object NeighorhoodStats {
  /**
   * Create a neighborhood statistics based on a move performed
   * @param searchResult results of performing a move
   * @param neighborhood move performed
   * @return statistics based on the move performed
   */
  def apply(searchResult: SearchResult, neighborhood: Neighborhood): NeighorhoodStats = {
    NeighorhoodStats(
      searchResult match {
        case NoMoveFound => false
        case MoveFound(_) => true
      },
      neighborhood.profiler.commonProfilingData._gain > 0,
      NeighborhoodUtils.slope(neighborhood),
      NeighborhoodUtils.lastCallDuration(neighborhood)
    )
  }
}