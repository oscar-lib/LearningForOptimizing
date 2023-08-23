package pdptw

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.neighborhood.{InsertPointUnroutedFirst, OnePointMove}
import oscar.cbls.core.search.{Best, First, Neighborhood}

class Solver(distancesMatrix: Array[Array[Long]], pdptw: VRP, obj: Objective) {

  val closestRelevantNeighbors = Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesMatrix,_ => pdptw.nodes)(_))


  def onePointInsert(k: Int, listOfPointsToInsert: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): Neighborhood = {
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = if(listOfPointsToInsert.isEmpty) pdptw.unrouted else () => listOfPointsToInsert.get,
      relevantPredecessor = () => pdptw.kFirst(k,closestRelevantNeighbors(_),_ => node => pdptw.isRouted(node)),
      vrp = pdptw,
      neighborhoodName = "InsertSinglePoint",
      hotRestart = hotRestart,
      selectNodeBehavior = if(best) Best() else First(),
      selectInsertionPointBehavior = if(best) Best() else First()
    )
  }

  def onePointMove(k: Int, listOfPointsToMove: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): Neighborhood = {
    OnePointMove(
      nodesToMove = if (listOfPointsToMove.isEmpty) pdptw.routed else () => listOfPointsToMove.get,
      relevantNewPredecessors = () => pdptw.kFirst(k, closestRelevantNeighbors(_), _ => node => pdptw.isRouted(node)),
      vrp = pdptw,
      neighborhoodName = "MoveSinglePoint",
      hotRestart = hotRestart,
      selectPointToMoveBehavior = if(best) Best() else First(),
      selectDestinationBehavior = if(best) Best() else First()
    )
  }

  def solve(): Unit = {
    val search = bestSlopeFirst(List(onePointInsert(10),onePointMove(20)))

    search.verbose = 1
    search.doAllMoves(obj = obj)

    println(pdptw.toString())
  }

}
