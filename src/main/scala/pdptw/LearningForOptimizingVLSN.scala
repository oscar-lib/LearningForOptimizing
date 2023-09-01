package pdptw

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood.{InsertPointRoutedFirst, InsertPointUnroutedFirst, OnePointMove, RemovePoint}
import oscar.cbls.core.search.{Best, First, Neighborhood}
import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing._
import oscar.cbls.lib.search.neighborhoods.vlsn.VLSN

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * VLSN also know as Very Large Scale Neighborhood is a special neighborhood that works
 * across multiple vehicles at the same time.
 *
 * It works in a black box, no need to deeply understand it to use it. But here is a short explanation.
 *
 * It tries to find a suite of complex moves impacting different vehicle at the same time.
 *
 * examples :
 * - Move couple A from vehicle 1 to vehicle 3,
 * 		- andThen move couple B from vehicle 3 to vehicle 4,
 * 		- andThen move couple C from vehicle 4 to vehicle 1
 * - Insert couple A in vehicle 1,
 * 		- andThen move couple B from vehicle 1 to vehicle 4,
 * 		- andThen remove couple C vehicle 4
 * 		Only the resulting composite move has to improve the solution.
 * @param pdptw
 * @param oscarModel
 */
case class LearningForOptimizingVLSN(pdptw: VRP, oscarModel: Model, closestRelevantPredecessors: Array[Iterable[Int]]) {


  val pickupToSummedDistanceToVehicles: List[(Int, Array[Long])] = oscarModel.precedences.map(pd => (pd._1, Array.tabulate(pdptw.v)(vehicle =>
    oscarModel.distanceAndTimeMatrix(pd._1)(vehicle) + oscarModel.distanceAndTimeMatrix(pd._1)(vehicle))))
  val pickupToxNearestVehicles = SortedMap.empty[Int, List[Int]] ++
    pickupToSummedDistanceToVehicles.map({
      case (chainHead, vehicleToDistance) =>
        (chainHead, KSmallest.getkSmallests((0 until pdptw.v).toArray, pdptw.v, (v: Int) => vehicleToDistance(v)))
    })



  def vlsn(l: Int = Int.MaxValue) = {
    val lClosestNeighborsByDistance: Array[SortedSet[Int]] = Array.tabulate(pdptw.n)(node =>
      SortedSet.empty[Int] ++ pdptw.kFirst(l, (node: Int) => closestRelevantPredecessors(node))(node))
    //VLSN neighborhood
    new VLSN(
      pdptw.v,
      () => pdptw.getVehicleToRouteMap.view.mapValues(_.filter(node => node >= pdptw.v && oscarModel.isPickupPoint(node))).toMap,
      () => SortedSet.empty[Int] ++ pdptw.unroutedNodes.filter(node => oscarModel.isPickupPoint(node)),
      nodeToRelevantVehicles = () => pickupToxNearestVehicles,

      targetVehicleNodeToInsertNeighborhood = oneChainInsertVLSN(lClosestNeighborsByDistance,l),
      targetVehicleNodeToMoveNeighborhood = moveChainVLSN(lClosestNeighborsByDistance,l),
      removeChainVLSN(),

      removeNodeAndReInsert = removeAndReInsertVLSN()(_),

      reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle) exhaustBack moveChainWithinVehicle(l,vehicle))),

      oscarModel.objPerVehicle,
      oscarModel.unroutedNodePenaltyObj,
      oscarModel.obj,
      name = s"VLSN($l)",
      reoptimizeAtStartUp = true,
      debugNeighborhoodExploration = false
    ) //most of the time, you do not want incremental VLSN
  }

  def oneChainInsertVLSN(lClosestNeighborsByDistance: Array[SortedSet[Int]], l: Int)(targetVehicle: Int): Int => Neighborhood = {

    val nodesOfTargetVehicle = SortedSet.empty[Int] ++ pdptw.getRouteOfVehicle(targetVehicle)

    (firstNodeOfUnroutedChain: Int) => {
      val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle intersect lClosestNeighborsByDistance(firstNodeOfUnroutedChain)

      val firstNodeOfChainInsertion =
        insertPointUnroutedFirst(
          () => List(firstNodeOfUnroutedChain),
          () => _ => lNearestNodesOfTargetVehicle,
          pdptw,
          hotRestart = false,
          selectInsertionPointBehavior = Best(),
          positionIndependentMoves = true, //compulsory because we are in VLSN
          neighborhoodName = "insertChainHead"
        )

      def lastNodeOfChainInsertion(lastNode: Int) = insertPointUnroutedFirst(
        () => List(lastNode),
        () => pdptw.kFirst(
          l,
          ChainsHelper.relevantNeighborsForLastNodeAfterHead( //TODO: filter in the target vehicle!!
            pdptw,
            oscarModel.chains)),
        pdptw,
        selectInsertionPointBehavior = Best(),
        positionIndependentMoves = true, //compulsory because we are in VLSN
        neighborhoodName = "insertChainLast")

      dynAndThen(firstNodeOfChainInsertion,
        (insertMove: InsertPointMove) => {
          lastNodeOfChainInsertion(oscarModel.chains.lastNodeInChainOfNode(insertMove.insertedPoint))
        }) name "insertChainVLSN"
    }
  }

  def moveChainVLSN(lClosestNeighborsByDistance: Array[SortedSet[Int]], l: Int)(targetVehicle: Int): Int => Neighborhood = {
    val nodesOfTargetVehicle = SortedSet.empty[Int] ++ pdptw.getRouteOfVehicle(targetVehicle)

    (chainHeadToMove: Int) => {
      val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle intersect lClosestNeighborsByDistance(chainHeadToMove)

      val firstNodeOfChainMove =
        onePointMove(
          () => List(chainHeadToMove),
          () => _ => lNearestNodesOfTargetVehicle,
          pdptw,
          selectDestinationBehavior = Best(),
          hotRestart = false,
          positionIndependentMoves = true, //compulsory because we are in VLSN
          neighborhoodName = "MoveChainHead"
        )

      def lastNodeOfChainMove(lastNode: Int) = onePointMove(
        () => List(lastNode),
        () => pdptw.kFirst(l,
          ChainsHelper.relevantNeighborsForLastNodeAfterHead(
            pdptw,
            oscarModel.chains
            //,Some(HashSet() ++ relevantPredecessors(lastNode))
          )), //TODO: takes a long time
        pdptw,
        positionIndependentMoves = true,
        neighborhoodName = "MoveChainLast")

      dynAndThen(firstNodeOfChainMove,
        (moveMove: OnePointMoveMove) =>
          lastNodeOfChainMove(oscarModel.chains.lastNodeInChainOfNode(moveMove.movedPoint))) name "OneChainMove"
    }
  }

  /*
    def a(chainHeadToMove: Int): Neighborhood = {
    val relevantNodesOfTargetVehicle = nodesOfTargetVehicle intersect (relevantPredecessors(chainHeadToMove))
    val lNearestNodesOfTargetVehicle = relevantNodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(chainHeadToMove) contains x)
   */

  def moveChainWithinVehicle(l: Int, vehicle: Int): Neighborhood = {
    val nodesOfTargetVehicle = SortedSet.empty[Int] ++ pdptw.getRouteOfVehicle(vehicle)
    val chainsHeadInVehicle = nodesOfTargetVehicle.filter(oscarModel.chains.isHead)

    val firstNodeOfChainMove =
      onePointMove(
        () => chainsHeadInVehicle,
        () => _ => nodesOfTargetVehicle,
        pdptw,
        selectDestinationBehavior = Best(),
        hotRestart = false,
        positionIndependentMoves = true, //compulsory because we are in VLSN
        neighborhoodName = "MoveChainHead"
      )

    def lastNodeOfChainMove(lastNode: Int) = onePointMove(
      () => List(lastNode),
      () => pdptw.kFirst(l,
        ChainsHelper.relevantNeighborsForLastNodeAfterHead(
          pdptw,
          oscarModel.chains
        )), //TODO: takes a long time
      pdptw,
      positionIndependentMoves = true,
      neighborhoodName = "MoveChainLast")

    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) =>
        lastNodeOfChainMove(oscarModel.chains.lastNodeInChainOfNode(moveMove.movedPoint))) name "OneChainMove"
  }

  def removeNode(pdptw: VRP, node: Int) = removePoint(
    () => List(node),
    pdptw,
    positionIndependentMoves = true,
    hotRestart = false)

  def removeChainVLSN()(chainHead: Int): Neighborhood = {
    mu[RemovePointMove, List[Int]](
      removeNode(pdptw, chainHead),
      //(List[(MoveType)], X) => Option[(Neighborhood, X)],
      (_, chainTail: List[Int]) => chainTail match {
        case Nil => None
        case h :: t => Some((removeNode(pdptw, h), t))
      },
      oscarModel.chains.chainOfNode(chainHead).tail,
      Int.MaxValue,
      intermediaryStops = false)
  }

  def removeAndReInsertVLSN()(headOfChainToRemove: Int): () => Unit = {
    val checkpointBeforeRemove = pdptw.routes.defineCurrentValueAsCheckpoint()
    require(headOfChainToRemove >= pdptw.v, s"cannot remove vehicle point: $headOfChainToRemove")

    val allNodesOfChain = oscarModel.chains.chainOfNode(headOfChainToRemove)
    for (nodeToRemove <- allNodesOfChain) {
      pdptw.routes.value.positionOfAnyOccurrence(nodeToRemove) match {
        case None => throw new Error(s"cannot remove non routed point: $nodeToRemove")
        case Some(positionOfPointToRemove) =>
          pdptw.routes.remove(positionOfPointToRemove)
      }
    }

    def restoreAndRelease: () => Unit = () => {
      pdptw.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
      pdptw.routes.releaseTopCheckpoint()
    }

    restoreAndRelease
  }

  //TODO: speedup this 3-opt; it eats most of the run time because Precedence is SSLLOOWWW
  //for re-optimization
  def threeOptOnVehicle(vehicle: Int) = {
    val nodesOfTargetVehicle = pdptw.getRouteOfVehicle(vehicle)
    //insertions points are position where we perform the insert,
    // basically the segment will start in place of the insertion point and the insertion point will be moved upward
    val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

    threeOpt(() => nodesOfTargetVehicle,
      () => _ => nodesOfTargetVehicleButVehicle,
      pdptw, breakSymmetry = false) filter ((t: ThreeOptMove) =>
      if (t.flipSegment) t.segmentEndPosition - t.segmentStartPosition < 4
      else math.min(math.abs(t.insertionPoint - t.segmentStartPosition), math.abs(t.insertionPoint - t.segmentEndPosition)) < 6)
  }

}
