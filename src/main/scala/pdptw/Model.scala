package pdptw

import oscar.cbls._
import oscar.cbls.business.routing.invariants.MovingVehicles
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.model.VRP

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


  private def generateVRPProblem(): VRP = {
		val store = new Store()
		val vrp = new VRP(store, n, v)
    vrp
  }

  private def generateObjectiveFunction(vrp: VRP): Objective = {

    val routeLengthsInvariant = RouteLength(vrp.routes, n, v, (from, to) => distanceAndTimeMatrix(from)(to))
    val movingVehiclesInvariant = MovingVehicles(vrp.routes, v)
    val unroutedNodePenalty = 1000000

    val obj = Objective(setSum(vrp.unrouted) * unroutedNodePenalty + setSum(movingVehiclesInvariant)*100 + sum(routeLengthsInvariant))
    vrp.m.close()
    obj
  }



}
