package pdptw

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood.{InsertPointUnroutedFirst, OnePointMove, RemovePoint}
import oscar.cbls.core.search.{Best, First, Neighborhood, SupportForAndThenChaining}
import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.lib.search.combinators.DynAndThen

case class SimpleNeighborhoods(pdptw: VRP, oscarModel: Model, closestRelevantNeighbors: Array[Iterable[Int]]) {


  def onePointInsert(k: Int, listOfPointsToInsert: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): InsertPointUnroutedFirst = {
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = if (listOfPointsToInsert.isEmpty) pdptw.unrouted else () => listOfPointsToInsert.get.filter(pdptw.isUnrouted),
      relevantPredecessor = () => pdptw.kFirst(k, closestRelevantNeighbors(_), _ => node => pdptw.isRouted(node)),
      vrp = pdptw,
      neighborhoodName = "InsertSinglePoint",
      hotRestart = hotRestart,
      selectNodeBehavior = if (best) Best() else First(),
      selectInsertionPointBehavior = if (best) Best() else First()
    )
  }

  def onePointMove(k: Int, listOfPointsToMove: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): OnePointMove = {
    OnePointMove(
      nodesToMove = if (listOfPointsToMove.isEmpty) pdptw.routed else () => listOfPointsToMove.get.filter(pdptw.isRouted),
      relevantNewPredecessors = () => pdptw.kFirst(k, closestRelevantNeighbors(_), _ => node => pdptw.isRouted(node)),
      vrp = pdptw,
      neighborhoodName = "MoveSinglePoint",
      hotRestart = hotRestart,
      selectPointToMoveBehavior = if (best) Best() else First(),
      selectDestinationBehavior = if (best) Best() else First()
    )
  }

  def removePoint(pointToRemove: Int): Neighborhood ={
    RemovePoint(() => List(pointToRemove), pdptw, "RemovePoint")
  }

  def couplePointInsert(k: Int, pickUpPointsToInsert: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): DynAndThen[InsertPointMove] = {
    val pickUpPointsToMoveNow =
      if (pickUpPointsToInsert.nonEmpty) pickUpPointsToInsert.get
      else oscarModel.pickupPointToDeliveryPoint.keys.toList
    dynAndThen(onePointInsert(k, Some(pickUpPointsToMoveNow), hotRestart, best), (move: InsertPointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.insertedPoint)
      onePointInsert(k, Some(List(deliveryPoint)), hotRestart, best)
    })
  }

  def couplePointMove(k: Int, pickUpPointsToMove: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): DynAndThen[OnePointMoveMove] = {
    val pickUpPointsToMoveNow =
      if(pickUpPointsToMove.nonEmpty) pickUpPointsToMove.get
      else oscarModel.pickupPointToDeliveryPoint.keys.toList
    dynAndThen(onePointMove(k, Some(pickUpPointsToMoveNow), hotRestart, best), (move: OnePointMoveMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.movedPoint)
      onePointInsert(k, Some(List(deliveryPoint)), hotRestart, best)
    })
  }

}
