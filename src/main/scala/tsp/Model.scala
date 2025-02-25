package tsp

import oscar.cbls.{CBLSIntVar, Objective, Store, setSum}
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.lib.invariant.numeric.Sum2
import oscar.cbls.lib.invariant.set.SetSum

object Model {

  def apply(problem: Problem): Model = {
    new Model(problem)
  }

}

class Model(val problem: Problem) {

  // city 0 is considered as the depot
  private val v                          = 1;
  private val n: Int                     = 0 + problem.nCities
  lazy val tsp                        = new VRP(new Store(), n, v, debug = false)
  private val oscarIdToTSPId: Array[Int] = Array.tabulate(n)(oscarId => Math.max(0, oscarId - v))

  lazy val distanceMatrix: Array[Array[Long]] =
    Array.tabulate(n)(from => {
      val fromId = from
      Array.tabulate(n)(to => {
        val toId = to
        problem.distances(fromId)(toId)
      })
    })

  // Invariant keeping the length of the tour
  val routeLengthInvariant: CBLSIntVar =
    RouteLength(tsp.routes, n, v, (from, to) => distanceMatrix(from)(to))(0)
  // invariant keeping the number of unrouted nodes
  val nUnroutedInvariant: SetSum = setSum(tsp.unrouted)


  lazy val objectiveFunction: Objective = generateObjectiveFunction(tsp: VRP)

  private def generateObjectiveFunction(vrp: VRP): Objective = {
    // To avoid empty route
    val unroutedNodePenalty = 1000000000
    // Cascading : if the first strong constraint is violated, no need to continue
    val obj = CascadingObjective(Sum2(nUnroutedInvariant * unroutedNodePenalty, routeLengthInvariant))
    //val obj = CascadingObjective(nUnroutedInvariant * unroutedNodePenalty, routeLengthInvariant)
    vrp.m.close()
    obj
  }

  override def toString: String = {

    s"\n\nResult\n" +
      s"=======\n" +
      s"Unrouted nodes : ${tsp.unrouted.value.size}\n" +
      s"Tour length : ${routeLengthInvariant.value.toDouble / problem.multiplierFactor}\n\t" +
      tsp
        .getRouteOfVehicle(0)
        .map(x => {
          s"${x}"
        })
        .mkString(" -> ")
  }

}
