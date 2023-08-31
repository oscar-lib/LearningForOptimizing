package pdptw

import oscar.cbls._
import oscar.cbls.business.routing.invariants.MovingVehicles
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.vehicleOfNodes
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
	lazy val distanceAndTimeMatrix: Array[Array[Long]] =
    Array.tabulate(n)(from => {
      val fromPosition =
        if (from < v) liLimProblem.vehicles(from).depot.positionXY
        else liLimProblem.nodes(oscarIdToLiLimId(from)).positionXY
      Array.tabulate(n)(to => {
        val toPosition =
          if (to < v) liLimProblem.vehicles(to).depot.positionXY
          else liLimProblem.nodes(oscarIdToLiLimId(to)).positionXY
        math.sqrt(Math.pow(fromPosition._1 - toPosition._1,2) + Math.pow(fromPosition._2 - toPosition._2,2)).ceil.toLong
      })
    })
  val routeLengthsInvariant: Array[CBLSIntVar] = RouteLength(pdpProblem.routes, n, v, (from, to) => distanceAndTimeMatrix(from)(to))
  val movingVehiclesInvariant: MovingVehicles = MovingVehicles(pdpProblem.routes, v)
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
  val timeWindowViolations: Array[CBLSIntVar] =
    Array.tabulate(v)(vehicle => CBLSIntVar(pdpProblem.m, 0, Domain.coupleToDomain(0, 1), s"TimeWindow constraint on vehicle $vehicle"))
  val timeWindowConstraint: TimeWindowConstraint =
    TimeWindowConstraint(pdpProblem.routes, n, v, timeWindows, distanceAndTimeMatrix, timeWindowViolations)

  //////////// Capacity ////////////

  // Capacity data
  val vehiclesCapacity: Array[Long] = liLimProblem.vehicles.map(_.capacity).toArray
  val contentVariationAtNode: Array[Long] = Array.fill(v)(0L) ++ liLimProblem.nodes.map(_.quantity).toArray
  // Constraint
  val vehicleCapacityViolations: Array[CBLSIntVar] =
    Array.tabulate(v)(vehicle => CBLSIntVar(pdpProblem.m, 0, name = s"TimeWindow constraint on vehicle $vehicle"))
  val vehicleCapacityConstraint: GlobalVehicleCapacityConstraint =
    new GlobalVehicleCapacityConstraint(pdpProblem.routes, n, v, vehiclesCapacity, contentVariationAtNode, vehicleCapacityViolations)

  //////////// Precedences ////////////

  // Precedences data
  val precedences: List[(Int,Int)] = liLimProblem.demands.map(d => (d.fromNodeId+v-1,d.toNodeId+v-1))
  val pickupPointToDeliveryPoint: HashMap[Int,Int] = HashMap.from(precedences)
  val deliveryPointToPickupPoint: HashMap[Int,Int] = HashMap.from(precedences.map(couple => (couple._2,couple._1)))
  val isPickupPoint: Array[Boolean] = Array.tabulate(n)(node => if (node < v) false else pickupPointToDeliveryPoint.keys.exists(_ == node))
  val isDeliveryPoint: Array[Boolean] = Array.tabulate(n)(node => if (node < v) false else deliveryPointToPickupPoint.keys.exists(_ == node))
	// Constraint
  val precedenceInvariant: Precedence = precedence(pdpProblem.routes, precedences)
  val precedencesConstraints = new ConstraintSystem(pdpProblem.m)
  for (start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(EQ(vehicleOfNodesNow(start), vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head)))
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
    s"\n\nResult\n" +
    	s"=======\n" +
      s"Unrouted nodes : ${pdpProblem.unrouted.value.size}\n" +
      s"Number of used vehicles : ${movingVehiclesInvariant.value.size}\n" +
      s"Total route length : ${routeLengthsInvariant.map(_.value).sum/liLimProblem.multFactor}\n\n" +
      movingVehiclesInvariant.value.toList.map(vehicle => pdpProblem.getRouteOfVehicle(vehicle).drop(1).map(x => x - v + 1).mkString(" ")).mkString("\n")
  }


}
