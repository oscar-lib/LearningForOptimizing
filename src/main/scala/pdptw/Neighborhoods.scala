package pdptw

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood.{InsertPointRoutedFirst, InsertPointUnroutedFirst, OnePointMove, RemovePoint}
import oscar.cbls.core.search.{Best, First, Neighborhood, SupportForAndThenChaining}
import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.lib.search.combinators.DynAndThen

import scala.util.Random

case class SimpleNeighborhoods(pdptw: VRP, oscarModel: Model, closestRelevantNeighbors: Array[Iterable[Int]]) {


  ///////////////////////////////////////
  //	One point insert/move/remove		 //
  ///////////////////////////////////////

  /** Generates a Neighborhood whose purpose is to insert a new node in the route.
   *
   * It first iterate over the point to insert and then iterate over the position.
   * Let say we have the points (4,5,6,7) to insert and the available position are after points (0,1,2,3,8,9,10)
   * It'll take the first point, 4 and try to insert it after 0 then after 1,2,3...
   * If no solution where found it'll try with the point 5 then 6...
   *
   * @param k The maximum number of position to test
   * @param listOfPointsToInsert And optional list of points to insert. If not specified it'll take the unrouted points.
   *                             Mainly used for one couple insert see below.
   * @param hotRestart If the algorithm restarts where it finished last time.
   *                   Lets say we insert 6.
   *                   If true it will try to insert 7 at the next round if false, it'll restart at 4.
   * @param best Test all the possible insertions and keep the best one. Take much more time.
   * @return The neighborhood that includes the above specifications
   */
  def onePointInsertUnroutedFirst(k: Int, listOfPointsToInsert: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): InsertPointUnroutedFirst = {
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = if (listOfPointsToInsert.isEmpty) pdptw.unrouted else () => listOfPointsToInsert.get.filter(pdptw.isUnrouted),
      relevantPredecessor = () => node => pdptw.kFirst(k, closestRelevantNeighbors(_), _ => node => pdptw.isRouted(node))(node).toList ::: pdptw.vehicles.toList,
      vrp = pdptw,
      neighborhoodName = "Insert Point - iterate first over point to insert",
      hotRestart = hotRestart,
      selectNodeBehavior = if (best) Best() else First(),
      selectInsertionPointBehavior = if (best) Best() else First()
    )
  }

  /** Generates a Neighborhood whose purpose is to insert a new node in the route.
   *
   * It first iterate over the position of insertion and then iterate over the point to insert.
   * Let say we have the points (4,5,6,7) to insert and the available position are after points (0,1,2,3,8,9,10)
   * It'll take the first insertion position, 0 and try to insert 4 after it, then 5, 6...
   * If no solution where found it'll try with the position 1, then 2, 3...
   *
   * @param k                    The maximum number of position to test
   * @param insertAfter 				And optional list of insertion position. If not specified it'll take the routed points.
   * @param hotRestart           If the algorithm restarts where it finished last time.
   *                             Lets say we inserted after point 2.
   *                             If true it will try to insert after point 3 at the next round if false, it'll restart try after point 0.
   * @param best                 Test all the possible insertions and keep the best one. Take much more time.
   * @return The neighborhood that includes the above specifications
   */
  def onePointInsertRoutedFirst(k: Int, insertAfter: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): InsertPointRoutedFirst = {
    InsertPointRoutedFirst(
      insertionPositions = if (insertAfter.isEmpty) pdptw.routed else () => insertAfter.get.filter(pdptw.isRouted),
      relevantSuccessorsToInsert = () => node => pdptw.kFirst(k, closestRelevantNeighbors(_), _ => node => pdptw.isUnrouted(node))(node).toList ::: pdptw.vehicles.toList,
      vrp = pdptw,
      neighborhoodName = "Insert Point - iterate first over position",
      hotRestart = hotRestart,
      selectInsertionPointBehavior = if (best) Best() else First(),
      selectInsertedNodeBehavior = if (best) Best() else First()
    )
  }

  /**
   * Moves one point to another position
   *
   * @param k The maximum number of positions to explore
   * @param pointsToMove An optional list of point to move. If not specified it'll take the routed points.
   *                     Mainly used for one couple move (see below)
   * @param hotRestart If the algorithm restarts where it finished last time.
   * @param best Test all the possible moves and keep the best one. Take much more time.
   * @return The neighborhood that includes the above specifications
   */
  def onePointMove(k: Int, pointsToMove: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): OnePointMove = {
    OnePointMove(
      nodesToMove = if (pointsToMove.isEmpty) pdptw.routed else () => pointsToMove.get.filter(pdptw.isRouted),
      relevantNewPredecessors = () => pdptw.kFirst(k, closestRelevantNeighbors(_), _ => node => pdptw.isRouted(node)),
      vrp = pdptw,
      neighborhoodName = "MoveSinglePoint",
      hotRestart = hotRestart,
      selectPointToMoveBehavior = if (best) Best() else First(),
      selectDestinationBehavior = if (best) Best() else First()
    )
  }

  /**
   * Removes one point from the route. Useful to get out of local minima.
   *
   * @param pointsToRemove	An optional list of points to remove. If None, it'll take the routed points.
   * @return A neighborhood that removes points from the route
   */
  def onePointRemove(pointsToRemove: Option[List[Int]]): RemovePoint = {
    RemovePoint(if (pointsToRemove.isEmpty) pdptw.routed else () => Random.shuffle(pointsToRemove.get.filter(pdptw.isRouted)),
      pdptw, "RemovePoint")
  }

  ///////////////////////////////////////
  //	Couple point insert/move/remove	 //
  ///////////////////////////////////////
	/* Due to precedence constraint, stipulating that either :
		- a pickup and it's delivery are routed on the same vehicle such that the pickup is before it's delivery
	 	- a pickup and it's delivery are unrouted

	 	We cannot route a pickup, validate the move and then (later) route it's delivery.
	 	We must to it at the same time. To do that we use the AndThen composite combinator.
	 	This combinator route the pickup andThen it's delivery in a  single composite move.
	 	(same goes for moving and removing)
	 */


  // Insert a couple of node using the onePointInsertUnroutedFirst for both pickup and it's delivery
  def couplePointInsertUnroutedFirst(k: Int, pickUpPointsToInsert: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): DynAndThen[InsertPointMove] = {
    val pickUpPointsToInsertNow =
      (if (pickUpPointsToInsert.nonEmpty) pickUpPointsToInsert.get
      else oscarModel.pickupPointToDeliveryPoint.keys.toList).filter(pdptw.isUnrouted)
    dynAndThen(onePointInsertUnroutedFirst(k, Some(pickUpPointsToInsertNow), hotRestart, best), (move: InsertPointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.insertedPoint)
      onePointInsertUnroutedFirst(k, Some(List(deliveryPoint)), hotRestart, best)
    })
  }

  // Insert a couple of node using the onePointInsertRoutedFirst for the pickup insertion and
  // the onePointInsertUnroutedFirst for it's delivery
  def couplePointInsertRoutedFirst(k: Int, pickUpPointsToInsert: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): DynAndThen[InsertPointMove] = {
    dynAndThen(onePointInsertRoutedFirst(k, None, hotRestart, best), (move: InsertPointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.insertedPoint)
      onePointInsertUnroutedFirst(k, Some(List(deliveryPoint)), hotRestart, best)
    })
  }

  // Move a couple of node at another place in the route.
  def couplePointMove(k: Int, pickUpPointsToMove: Option[List[Int]] = None, hotRestart: Boolean = false, best: Boolean = false): DynAndThen[OnePointMoveMove] = {
    val pickUpPointsToMoveNow =
      (if(pickUpPointsToMove.nonEmpty) pickUpPointsToMove.get
      else oscarModel.pickupPointToDeliveryPoint.keys.toList).filter(pdptw.isRouted)
    dynAndThen(onePointMove(k, Some(pickUpPointsToMoveNow), hotRestart, best), (move: OnePointMoveMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.movedPoint)
      onePointInsertUnroutedFirst(k, Some(List(deliveryPoint)), hotRestart, best)
    })
  }

  // Remove a couple of node.
  // BEWARE that this Neighborhood as is will never generate accepted moves because
  // the resulting move will deteriorate the objective function.
  // It has to be encapsulate in a combinator that accept all moves (see multipleRemoveCouples (==> .acceptAll()))
  def removeCouple(pickUpPointsToRemove: Option[List[Int]] = None): DynAndThen[RemovePointMove] ={
    val pickUpPointsToMoveNow =
      if (pickUpPointsToRemove.nonEmpty) pickUpPointsToRemove.get
      else oscarModel.pickupPointToDeliveryPoint.keys.toList
    dynAndThen(onePointRemove(Some(pickUpPointsToMoveNow)), (move: RemovePointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.pointToRemove)
      onePointRemove(Some(List(deliveryPoint)))
    })
  }

  // In a single movement remove nbOfCoupleToRemove couples
  def multipleRemoveCouples(nbOfCoupleToRemove: Int): Neighborhood = {
    atomic(removeCouple().acceptAll(), _ > nbOfCoupleToRemove)
  }

}
