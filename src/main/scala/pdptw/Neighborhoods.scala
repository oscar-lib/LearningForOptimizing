package pdptw

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood.{InsertPointRoutedFirst, InsertPointUnroutedFirst, OnePointMove, RemovePoint}
import oscar.cbls.core.search.{Best, First, Neighborhood}
import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.lib.search.combinators.Atomic

import scala.util.Random

case class SimpleNeighborhoods(pdptw: VRP,
                               oscarModel: Model,
                               closestRelevantPredecessors: Array[Iterable[Int]],
                               closestRelevantSuccessors: Array[Iterable[Int]]) {

  val routedPickups: () => Iterable[Int] =
    () => pdptw.routed().intersect(oscarModel.pickupPointToDeliveryPoint.keys.toSet)
  val unRoutedPickups: () => Iterable[Int] =
    () => pdptw.unrouted().intersect(oscarModel.pickupPointToDeliveryPoint.keys.toSet)

  private def randVehicle(): () => Int ={
    () => Random.shuffle(Array.tabulate(pdptw.v)(x => x).filter(x => pdptw.getRouteOfVehicle(x).drop(1).nonEmpty)).head
  }


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
   * @param listOfPointsToInsert A function returning the list of points to insert.
   *                             Mainly used for one couple insert see below.
   * @param hotRestart If the algorithm restarts where it finished last time.
   *                   Lets say we insert 6.
   *                   If true it will try to insert 7 at the next round if false, it'll restart at 4.
   * @param best Test all the possible insertions and keep the best one. Take much more time.
   * @return The neighborhood that includes the above specifications
   */
  def onePointInsertUnroutedFirst(k: Int,
                                  listOfPointsToInsert: () => Iterable[Int],
                                  hotRestart: Boolean = false,
                                  best: Boolean = false): InsertPointUnroutedFirst = {
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = listOfPointsToInsert,
      relevantPredecessor = () => node => pdptw.kFirst(k, closestRelevantPredecessors(_), _ => node => pdptw.isRouted(node))(node).toList ::: pdptw.vehicles.toList,
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
   * @param insertAfter 				 A function returning the list of insertion position.
   * @param hotRestart           If the algorithm restarts where it finished last time.
   *                             Lets say we inserted after point 2.
   *                             If true it will try to insert after point 3 at the next round if false, it'll restart try after point 0.
   * @param best                 Test all the possible insertions and keep the best one. Take much more time.
   * @return The neighborhood that includes the above specifications
   */
  def onePointInsertRoutedFirst(k: Int,
                                insertAfter: () => Iterable[Int],
                                hotRestart: Boolean = false,
                                best: Boolean = false): InsertPointRoutedFirst = {
    InsertPointRoutedFirst(
      insertionPositions = insertAfter,
      relevantSuccessorsToInsert = () => node => pdptw.kFirst(k, closestRelevantSuccessors(_), _ => node => pdptw.isUnrouted(node) && oscarModel.isPickupPoint(node))(node).toList,
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
   * @param pointsToMove A function returning the list of point to move.
   *                     If not specified it'll take the routed points.
   *                     Mainly used for one couple move (see below)
   * @param hotRestart If the algorithm restarts where it finished last time.
   * @param best Test all the possible moves and keep the best one. Take much more time.
   * @return The neighborhood that includes the above specifications
   */
  def onePointMove(k: Int,
                   pointsToMove: () => Iterable[Int] = pdptw.routed,
                   hotRestart: Boolean = false,
                   best: Boolean = false): OnePointMove = {
    OnePointMove(
      nodesToMove = pointsToMove,
      relevantNewPredecessors = () => pdptw.kFirst(k, closestRelevantPredecessors(_), _ => node => pdptw.isRouted(node)),
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
   * @param pointsToRemove	A function returning the list of points to remove.
   *                       If not specified, it'll take the routed points.
   * @return A neighborhood that removes points from the route
   */
  def onePointRemove(pointsToRemove: () => Iterable[Int] = pdptw.routed): RemovePoint = {
    RemovePoint(() => pointsToRemove(), pdptw, "RemovePoint")
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
  def couplePointInsertUnroutedFirst(k: Int,
                                     pickUpPointsToInsert: () => Iterable[Int] = unRoutedPickups,
                                     hotRestart: Boolean = false,
                                     best: Boolean = false): Neighborhood = {
    dynAndThen(onePointInsertUnroutedFirst(k, pickUpPointsToInsert, hotRestart, best), (move: InsertPointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.insertedPoint)
      onePointInsertUnroutedFirst(k, () => List(deliveryPoint), hotRestart, best)
    }) name "One couple insert unrouted first"
  }

  // Insert a couple of node using the onePointInsertRoutedFirst for the pickup insertion and
  // the onePointInsertUnroutedFirst for it's delivery
  def couplePointInsertRoutedFirst(k: Int,
                                   insertAfterPoints: () => Iterable[Int] = pdptw.routed,
                                   hotRestart: Boolean = false,
                                   best: Boolean = false): Neighborhood = {
    dynAndThen(onePointInsertRoutedFirst(k, insertAfterPoints, hotRestart, best), (move: InsertPointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.insertedPoint)
      onePointInsertUnroutedFirst(k, () => List(deliveryPoint), hotRestart, best)
    }) name "One couple insert routed first"
  }

  // Move a couple of node at another place in the route.
  def couplePointMove(k: Int,
                       pickUpPointsToMove: () => Iterable[Int] = routedPickups,
                       hotRestart: Boolean = false,
                       best: Boolean = false): Neighborhood = {
    dynAndThen(onePointMove(k, pickUpPointsToMove, hotRestart, best), (move: OnePointMoveMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.movedPoint)
      onePointMove(k, () => List(deliveryPoint), hotRestart, best)
    }) name "One couple move"
  }

  // Remove a couple of node.
  // BEWARE that this Neighborhood as is will never generate accepted moves because
  // the resulting move will deteriorate the objective function.
  // It has to be encapsulate in a combinator that accept all moves (see multipleRemoveCouples (==> .acceptAll()))
  def couplePointRemove(pickUpPointsToRemove: () => Iterable[Int] =
                   routedPickups): Neighborhood ={
    dynAndThen(onePointRemove(() => pickUpPointsToRemove()), (move: RemovePointMove) => {
      val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.pointToRemove)
      onePointRemove(() => List(deliveryPoint))
    }) name "One couple remove"
  }

  // In a single movement remove nbOfCoupleToRemove couples
  def multipleCouplesPointRemove(nbOfCoupleToRemove: Int): Neighborhood = {
    atomic(couplePointRemove().acceptAll(), _ > nbOfCoupleToRemove)
  }

  // Remove all couples of the specified vehicle
  def emptyVehicle(vehicle: () => Int = randVehicle()): Neighborhood ={
    var pickUpsToRemove: List[Int] = List.empty
    var nbOfPickupToRemove: Int = 0
    atomic(
      dynAndThen(onePointRemove(() => pickUpsToRemove), (move: RemovePointMove) => {
        val deliveryPoint = oscarModel.pickupPointToDeliveryPoint(move.pointToRemove)
        onePointRemove(() => List(deliveryPoint))
    }).acceptAll(), _ > nbOfPickupToRemove) onQuery {
      pickUpsToRemove = pdptw.getRouteOfVehicle(vehicle()).intersect(oscarModel.pickupPointToDeliveryPoint.keys.toList)
      nbOfPickupToRemove = pickUpsToRemove.length
    } name "Empty route of vehicle"
  }

  // Removes all couples of several vehicles (random vehicle selection)
  def emptyMultiplesVehicle(nbOfVehicleToEmpty: Int): Neighborhood = {
    Atomic(emptyVehicle(),_ > nbOfVehicleToEmpty)
  }

}
