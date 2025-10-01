package tsp

import oscar.cbls.{cardinality, CBLSIntVar, Objective, Store}
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.lib.invariant.numeric.Sum2
import bridge.SerializableModel
import bridge.MessageType
import upickle.default._

object Model {

  def apply(problem: Problem): Model = {
    new Model(problem)
  }

}

class Model(val problem: Problem) extends SerializableModel {
  implicit val problemRW: ReadWriter[Problem] = macroRW
  // all nodes in the problem, including the depot
  private val n: Int = problem.nCities()
  lazy val tsp       = new VRP(new Store(), n, 1, debug = false)
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
    RouteLength(tsp.routes, n, 1, (from, to) => distanceMatrix(from)(to))(0)
  // invariant keeping the number of unrouted nodes
  val nUnroutedInvariant                = cardinality(tsp.unrouted)
  lazy val objectiveFunction: Objective = generateObjectiveFunction(tsp: VRP)
  // Normalize by the maximal distance between two cities
  override def getNormalizationFactor(): Float = {
    return 1 / (this.distanceMatrix.flatten.max.toFloat / this.problem.multiplierFactor.toFloat)
  }

  /** Generates an objective function, minimizing the number of unrouted nodes and the traveled
    * distance
    * @param vrp
    *   routing problem to optimize
    * @return
    *   objective function minimizing the number of unrouted nodes and the traveled distance
    */
  private def generateObjectiveFunction(vrp: VRP): Objective = {
    // To avoid empty route
    val unroutedNodePenalty = 1000000000
    // Cascading : if the first strong constraint is violated, no need to continue
    val obj = CascadingObjective(
      Sum2(nUnroutedInvariant * unroutedNodePenalty, routeLengthInvariant)
    )
    // val obj = CascadingObjective(nUnroutedInvariant * unroutedNodePenalty, routeLengthInvariant)
    vrp.m.close()
    obj
  }

  override def hasObjectivePenalty(): Boolean = {
    nUnroutedInvariant.value > 0
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

  override def getJSONState(): String = {
    val routes: List[List[Int]] = (0 until this.tsp.v)
      .map(vehicle => this.tsp.getRouteOfVehicle(vehicle))
      .filter(_.length > 1)
      .toList
    return upickle.default.write(routes)
  }

  override def getJSONStaticProblemData(): String = {
    write(this.problem)
  }

  override def getProblemCode(): MessageType.Value = MessageType.STATIC_DATA_TSP
}
