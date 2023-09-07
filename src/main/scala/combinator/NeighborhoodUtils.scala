package combinator

import oscar.cbls.core.search.{Neighborhood,NeighborhoodCombinator}
import oscar.cbls.lib.search.combinators.DynAndThen
import oscar.cbls.core.search.profiling.ProfilingData

object NeighborhoodUtils {
  private def getProfiler(neighborhood : Neighborhood) : ProfilingData = {
    neighborhood match {
      // DynAndThen does not set correctly the profiler information, need to cast to retrieve it correctly
      case n: NeighborhoodCombinator =>
        if (n.subNeighborhoods.length == 1) {
          n.subNeighborhoods.head match {
            case dat: DynAndThen[_] =>
              dat.profiler.commonProfilingData
            case _ =>
              n.profiler.commonProfilingData
          }
        } else {
          n.profiler.commonProfilingData
        }
      case _ =>
        neighborhood.profiler.commonProfilingData
    }
  }

}
