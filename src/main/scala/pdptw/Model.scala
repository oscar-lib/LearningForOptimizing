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

import scala.collection.immutable.HashMap

object Model {

  def apply(liLimProblem: LiLimProblem): Model = {
		new Model(liLimProblem)
  }
}

class Model(liLimProblem: LiLimProblem) {

  private val v: Int = liLimProblem.vehicles.length
  private val n: Int = v + liLimProblem.nodes.length
  private val oscarIdToLiLimId: Array[Int] = Array.tabulate(n)(oscarId => Math.max(0,oscarId-v))

  // Quote : "The value of travel time is equal to the value of distance."
	lazy val distanceAndTimeMatrix: Array[Array[Long]] =
    Array.tabulate(n)(from => {
      val fromNode =
        if (from < v) liLimProblem.vehicles(from).depot.positionXY
        else liLimProblem.nodes(oscarIdToLiLimId(from)).positionXY
      Array.tabulate(n)(to => {
        val toNode =
          if (to < v) liLimProblem.vehicles(to).depot.positionXY
          else liLimProblem.nodes(oscarIdToLiLimId(to)).positionXY
        math.sqrt(Math.pow(fromNode._1 - toNode._1,2) + Math.pow(fromNode._2 - toNode._2,2)).ceil.toLong
      })
    })
  lazy val pdpProblem: VRP = generateVRPProblem()
  lazy val objectiveFunction: Objective = generateObjectiveFunction(pdpProblem: VRP)

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

  val vehiclesCapacity: Array[Long] = liLimProblem.vehicles.map(_.capacity).toArray
  val contentVariationAtNode: Array[Long] = Array.fill(v)(0L) ++ liLimProblem.nodes.map(_.quantity).toArray
  val precedences: List[(Int,Int)] = liLimProblem.demands.map(d => (d.fromNodeId+v-1,d.toNodeId+v-1))
  val pickupPointToDeliveryPoint: HashMap[Int,Int] = HashMap.from(precedences)
  val deliveryPointToPickupPoint: HashMap[Int,Int] = HashMap.from(precedences.map(couple => (couple._2,couple._1)))


  private def generateVRPProblem(): VRP = {
		val store = new Store()
		val vrp = new VRP(store, n, v, debug = false)
    vrp
  }

  private def generateObjectiveFunction(vrp: VRP): Objective = {

    val routeLengthsInvariant = RouteLength(vrp.routes, n, v, (from, to) => distanceAndTimeMatrix(from)(to))
    val movingVehiclesInvariant = MovingVehicles(vrp.routes, v)
    val timeWindowViolations = Array.tabulate(v)(vehicle => CBLSIntVar(vrp.m,0,Domain.coupleToDomain(0,1),s"TimeWindow constraint on vehicle $vehicle"))
    val timeWindowConstraint = TimeWindowConstraint(vrp.routes,n,v,timeWindows,distanceAndTimeMatrix,timeWindowViolations)
    val vehicleCapacityViolations = Array.tabulate(v)(vehicle => CBLSIntVar(vrp.m,0,name = s"TimeWindow constraint on vehicle $vehicle"))
    val vehicleCapacityConstraint = new GlobalVehicleCapacityConstraint(vrp.routes,n,v,vehiclesCapacity,contentVariationAtNode, vehicleCapacityViolations)
    val precedenceInvariant = precedence(vrp.routes, precedences)
    val vehicleOfNodesNow = vehicleOfNodes(vrp.routes, v)
    val precedencesConstraints = new ConstraintSystem(vrp.m)
    for (start <- precedenceInvariant.nodesStartingAPrecedence)
      precedencesConstraints.add(EQ(vehicleOfNodesNow(start), vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head)))
    precedencesConstraints.add(EQ(0, precedenceInvariant))
    val unroutedNodePenalty = 1000000000

    val obj = CascadingObjective(
      sum(vehicleCapacityViolations),
      sum(timeWindowViolations),
      precedencesConstraints,
      setSum(vrp.unrouted, x => 1) * unroutedNodePenalty + setSum(movingVehiclesInvariant, x => 1)*1000000 + sum(routeLengthsInvariant))
    vrp.m.close()
    obj
  }



}
