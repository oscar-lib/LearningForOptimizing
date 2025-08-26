package tsp

import oscar.cbls.{CBLSIntVar, Objective, Store, cardinality}
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.lib.invariant.numeric.Sum2

object Model {

  def apply(problem: Problem): Model = {
    new Model(problem)
  }

}

class Model(val problem: Problem) {

  // city 0 is considered as the depot
  private val v                          = 1;
  // all nodes in the problem, including the depot
  private val n: Int                     = 0 + problem.nCities
  lazy val tsp                        = new VRP(new Store(), n, v, debug = false)
  // distance between cities
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
  val nUnroutedInvariant = cardinality(tsp.unrouted)

  lazy val objectiveFunction: Objective = generateObjectiveFunction(tsp: VRP)

  /**
   * Generates an objective function, minimizing the number of unrouted nodes and the traveled distance
   * @param vrp routing problem to optimize
   * @return objective function minimizing the number of unrouted nodes and the traveled distance
   */
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
