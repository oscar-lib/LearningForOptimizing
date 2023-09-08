package pdptw

import oscar.cbls._
import oscar.cbls.business.routing.invariants.MovingVehicles
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.extensions.Chains
import oscar.cbls.business.routing.{ChainsHelper, vehicleOfNodes}
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.lib.constraint.EQ
import oscar.cbls.lib.invariant.seq.Precedence

import scala.collection.immutable.HashMap

object Model {

  def apply(liLimProblem: LiLimProblem): Model = {
		new Model(liLimProblem)
  }
}

class Model(liLimProblem: LiLimProblem) {

  //////////// VRP ////////////
  private val v: Int = liLimProblem.vehicles.length
  private val n: Int = v + liLimProblem.nodes.length
  lazy val pdpProblem: VRP = new VRP(new Store(), n, v, debug = false)

  private val oscarIdToLiLimId: Array[Int] = Array.tabulate(n)(oscarId => Math.max(0,oscarId-v))

  // Quote : "The value of travel time is equal to the value of distance."
  val nodePositions: Array[(Int,Int)] = Array.tabulate(n)(node => {
    if(node < v) liLimProblem.vehicles(node).depot.positionXY
    else liLimProblem.nodes(oscarIdToLiLimId(node)).positionXY
  })
	lazy val distanceAndTimeMatrix: Array[Array[Long]] =
    Array.tabulate(n)(from => {
      val fromPosition = nodePositions(from)
      Array.tabulate(n)(to => {
        val toPosition = nodePositions(to)
        math.sqrt(Math.pow(fromPosition._1 - toPosition._1,2) + Math.pow(fromPosition._2 - toPosition._2,2)).ceil.toLong
      })
    })

  // Invariant keeping the lenght of each vehicle's route
  val routeLengthsInvariant: Array[CBLSIntVar] = RouteLength(pdpProblem.routes, n, v, (from, to) => distanceAndTimeMatrix(from)(to))
  // Invariant keeping the moving vehicle in a Set
  val movingVehiclesInvariant: MovingVehicles = MovingVehicles(pdpProblem.routes, v)
  // Invariant, for each node we maintain it's current vehicle (v if not routed)
  val vehicleOfNodesNow: Array[CBLSIntVar] = vehicleOfNodes(pdpProblem.routes, v)

  lazy val objectiveFunction: Objective = generateObjectiveFunction(pdpProblem: VRP)

  //////////// Time Windows ////////////

  // Time window data
  lazy val timeWindows: Array[TransferFunction] = Array.tabulate(n)(node =>
    if (node < v) {
      TransferFunction.identifyTransferFunction(node)
    } else {
      TransferFunction.createFromEarliestAndLatestArrivalTime(
        node,
        liLimProblem.nodes(node - v).earliestArrival,
        liLimProblem.nodes(node - v).latestArrival,
        liLimProblem.nodes(node - v).duration)
    }
  )
  // Constraint
  // An array keeping the time window violation (0 ==> no violation, 1 ==> violation) of each vehicle
  val timeWindowViolations: Array[CBLSIntVar] =
    Array.tabulate(v)(vehicle => CBLSIntVar(pdpProblem.m, 0, Domain.coupleToDomain(0, 1), s"TimeWindow constraint on vehicle $vehicle"))
  // The constraint maintaining the above invariant
  val timeWindowConstraint: TimeWindowConstraint =
    TimeWindowConstraint(pdpProblem.routes, n, v, timeWindows, distanceAndTimeMatrix, timeWindowViolations)

  //////////// Capacity ////////////

  // Capacity data, vehicle capacity and the charge picked up/dropped off at each point
  val vehiclesCapacity: Array[Long] = liLimProblem.vehicles.map(_.capacity).toArray
  val contentVariationAtNode: Array[Long] = Array.fill(v)(0L) ++ liLimProblem.nodes.map(_.quantity).toArray
  // Constraint
  // An array keeping the capacity violation (0 ==> no violation, 1 ==> violation) of each vehicle
  val vehicleCapacityViolations: Array[CBLSIntVar] =
    Array.tabulate(v)(vehicle => CBLSIntVar(pdpProblem.m, 0, name = s"Capacity constraint on vehicle $vehicle"))
  // The constraint maintaining the above invariant
  val vehicleCapacityConstraint: GlobalVehicleCapacityConstraint =
    new GlobalVehicleCapacityConstraint(pdpProblem.routes, n, v, vehiclesCapacity, contentVariationAtNode, vehicleCapacityViolations)

  //////////// Precedences ////////////

  // Precedences data
  val precedences: List[(Int,Int)] = liLimProblem.demands.map(d => (d.fromNodeId+v-1,d.toNodeId+v-1))
  val chains: Chains = new Chains(pdpProblem,precedences.map(p => List(p._1,p._2)))
  def pickupPointToDeliveryPoint(pickup: Int): Int = chains.lastNodeInChainOfNode(pickup)
  def deliveryPointToPickupPoint(delivery: Int): Int = chains.firstNodeInChainOfNode(delivery)
  def isPickupPoint(node: Int): Boolean = chains.isHead(node)
  def isDeliveryPoint(node: Int): Boolean = chains.isLast(node)
	// Constraint
  val precedenceInvariant: Precedence = precedence(pdpProblem.routes, precedences)
  // Ensuring that pickup and dropoff node are on same vehicle.
  val precedencesConstraints = new ConstraintSystem(pdpProblem.m)
  for (start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(EQ(vehicleOfNodesNow(start), vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head)))
  // Ensuring that dropOff node follows pickUp node
  precedencesConstraints.add(EQ(0, precedenceInvariant))


  private def generateObjectiveFunction(vrp: VRP): Objective = {
    // To avoid empty route
    val unroutedNodePenalty = 1000000000
    val usedVehiclePenalty = 1000000

    // Cascading : if the first strong constraint is violated, no need to continue
    val obj = CascadingObjective(
      sum(vehicleCapacityViolations),
      sum(timeWindowViolations),
      precedencesConstraints,
      // nb of unrouted nodes times penalty + nb of moving vehicles times penalty + route length
      setSum(vrp.unrouted, x => 1) * unroutedNodePenalty + setSum(movingVehiclesInvariant, x => 1)*usedVehiclePenalty + sum(routeLengthsInvariant))
    vrp.m.close()
    obj
  }

  override def toString: String = {
    case class PointData(coupleId : Int,
      routingId : Int,
      problemId : Int,
      prefix : String)

    val pointsData : Array[Option[PointData]] = (0 until n).toArray.map(nId => {
      if (nId < v)
        None
      else {
        val coupleAndIndex = liLimProblem.demands.zipWithIndex.filter(p => (p._1.fromNodeId + v - 1) == nId || (p._1.toNodeId + v -1) == nId)(0)
        Some(PointData(coupleAndIndex._2,
          nId,
          nId-v+1,
          if (coupleAndIndex._1.fromNodeId + v -1 == nId) "p" else "d"))
        }
    })

    s"\n\nResult\n" +
    s"=======\n" +
    s"Unrouted nodes : ${pdpProblem.unrouted.value.size}\n" +
    s"Number of used vehicles : ${movingVehiclesInvariant.value.size}\n" +
    s"Total route length : ${routeLengthsInvariant.map(_.value).sum.toDouble/liLimProblem.multFactor}\n\n" +
    movingVehiclesInvariant.value.toList.map(vehicle => {
      pdpProblem.getRouteOfVehicle(vehicle).drop(1).map(x => {
        val pData = pointsData(x).get
//        s"${pData.prefix}${pData.coupleId}(${pData.routingId},${pData.problemId})"
        s"${pData.prefix}${pData.coupleId}"
      }).mkString("->")
    }).mkString("\n")
//      movingVehiclesInvariant.value.toList.map(vehicle => pdpProblem.getRouteOfVehicle(vehicle).drop(1).map(x => x - v + 1).mkString(" ")).mkString("\n")
  }


}
