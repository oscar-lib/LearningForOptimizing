package tsp

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood.{
  InsertPointUnroutedFirst,
  OnePointMove,
  RemovePoint,
  TwoOpt
}
import oscar.cbls.core.search.{Best, First, Neighborhood}
import oscar.cbls.lib.search.combinators.Atomic

case class SimpleNeighborhoods(tsp: VRP, oscarModel: Model) {

  val allNodes: List[Int]            = (0 until tsp.n).toList
  val allNodesExceptDepot: List[Int] = (1 until tsp.n).toList // does not count city 0
  // predecessor candidates for a node: all nodes except the node itself
  val predecessorForNode: Map[Int, Iterable[Int]] =
    Array.tabulate(tsp.n)(node => node -> allNodes.filter((neighbor => neighbor != node))).toMap
  // successor candidates for a node: all nodes except the node itself and the depot
  val successorForNode: Map[Int, Iterable[Int]] =
    Array
      .tabulate(tsp.n)(node => node -> allNodesExceptDepot.filter((neighbor => neighbor != node)))
      .toMap
  val unroutedNode: () => Iterable[Int] =
    () => tsp.unrouted()

  /** Generates a Neighborhood whose purpose is to insert a new node in the route.
    *
    * It first iterate over the point to insert and then iterate over the position. Let say we have
    * the points (4,5,6,7) to insert and the available position are after points (0,1,2,3,8,9,10)
    * It'll take the first point, 4 and try to insert it after 0 then after 1,2,3... If no solution
    * where found it'll try with the point 5 then 6...
    *
    * @param k
    *   The maximum number of position to test
    * @param listOfPointsToInsert
    *   A function returning the list of points to insert. Mainly used for one couple insert see
    *   below.
    * @param hotRestart
    *   If the algorithm restarts where it finished last time. Lets say we insert 6. If true it will
    *   try to insert 7 at the next round if false, it'll restart at 4.
    * @param best
    *   Test all the possible insertions and keep the best one. Take much more time.
    * @return
    *   The neighborhood that includes the above specifications
    */
  def insertNode(
    k: Int,
    hotRestart: Boolean = false,
    best: Boolean = false
  ): InsertPointUnroutedFirst = {
    InsertPointUnroutedFirst(
      tsp.unrouted,
      relevantPredecessor = () =>
        tsp
          .kFirst(k, predecessorForNode, _ => node => tsp.isRouted(node)),
      vrp = tsp,
      neighborhoodName = s"insert_$k - ${if (best) "best" else "first"}",
      hotRestart = hotRestart,
      selectInsertionPointBehavior = if (best) Best() else First()
    )
  }

  /** Generates a Neighborhood whose purpose is to move a routed node elsewhere in the route.
    *
    * It first iterate over the point to nodes to move and then iterate over the position. Let say
    * we have the points (4,5,6,7) to move and the available position are after points
    * (0,1,2,3,8,9,10) It'll take the first point, 4 and try to move it after 0 then after 1,2,3...
    * If no solution where found it'll try with the point 5 then 6...
    *
    * @param k
    *   The maximum number of position to test
    * @param listOfPointsToInsert
    *   A function returning the list of points to insert. Mainly used for one couple insert see
    *   below.
    * @param hotRestart
    *   If the algorithm restarts where it finished last time. Lets say we insert 6. If true it will
    *   try to insert 7 at the next round if false, it'll restart at 4.
    * @param best
    *   Test all the possible insertions and keep the best one. Take much more time.
    * @return
    *   The neighborhood that includes the above specifications
    */
  def moveOneNode(
    k: Int,
    listOfPointsToInsert: () => Iterable[Int] = unroutedNode,
    hotRestart: Boolean = false,
    best: Boolean = false
  ): OnePointMove = {
    OnePointMove(
      tsp.routed,
      relevantNewPredecessors = () =>
        node =>
          tsp
            .kFirst(k, predecessorForNode, _ => node => tsp.isRouted(node))(node)
            .toList ::: tsp.vehicles.toList,
      vrp = tsp,
      neighborhoodName = s"move_$k - ${if (best) "best" else "first"}",
      hotRestart = hotRestart,
      selectPointToMoveBehavior = if (best) Best() else First()
    )
  }

  /** Generates a Neighborhood performing a 2-opt in the route.
    *
    * @param k
    *   The maximum number of position to test
    * @param listOfPointsToInsert
    *   A function returning the list of points to insert. Mainly used for one couple insert see
    *   below.
    * @param hotRestart
    *   If the algorithm restarts where it finished last time. Lets say we insert 6. If true it will
    *   try to insert 7 at the next round if false, it'll restart at 4.
    * @param best
    *   Test all the possible insertions and keep the best one. Take much more time.
    * @return
    *   The neighborhood that includes the above specifications
    */
  def twoOpt(
    k: Int,
    listOfPointsToInsert: () => Iterable[Int] = unroutedNode,
    hotRestart: Boolean = false,
    best: Boolean = false
  ): TwoOpt = {
    TwoOpt(
      tsp.routed,
      relevantNewSuccessors = () =>
        node =>
          tsp
            .kFirst(k, successorForNode, _ => node => tsp.isRouted(node))(node)
            .toList ::: tsp.vehicles.toList,
      vrp = tsp,
      neighborhoodName = s"2opt_$k - ${if (best) "best" else "first"}",
      hotRestart = hotRestart,
      selectSegmentStartBehavior = if (best) Best() else First()
    )
  }

  /** Removes at most n routed nodes from the path
    *
    * @param n
    *   The maximum number of nodes to remove
    * @return
    *   The neighborhood that includes the above specifications
    */
  def removeNode(n: Int): Neighborhood = {
    Atomic(RemovePoint(tsp.routed, tsp), nbIt => nbIt >= Math.min(n, tsp.getRouteOfVehicle(0).size))
  }

}
