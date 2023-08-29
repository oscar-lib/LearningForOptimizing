package pdptw

import combinator.BestSlopeFirstLearningWay
import oscar.cbls._
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.lib.search.combinators.BestSlopeFirst

case class Solver(oscarModel: Model) {
  private val distancesAndTimeMatrix: Array[Array[Long]] = oscarModel.distanceAndTimeMatrix
  private val pdptw: VRP = oscarModel.pdpProblem
  private val obj: Objective = oscarModel.objectiveFunction

  // Relevant predecessors definition for each node (here any node can be the predecessor of another node)
  val relevantPredecessorsOfNodes =
    TransferFunction.relevantPredecessorsOfNodes(pdptw.n, pdptw.v, oscarModel.timeWindows, oscarModel.distanceAndTimeMatrix)
  // Sort them lazily by distance
  val closestRelevantNeighborsByDistance =
    Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(oscarModel.distanceAndTimeMatrix, relevantPredecessorsOfNodes)(_))

  private val simpleNeighborhoods = SimpleNeighborhoods(pdptw, oscarModel, closestRelevantNeighborsByDistance)


  def solve(verbosity: Int): Unit = {
    val search =
      new BestSlopeFirstLearningWay(
        List(
          simpleNeighborhoods.couplePointInsertUnroutedFirst(pdptw.n/2),
          simpleNeighborhoods.couplePointMove(pdptw.n/2),
          simpleNeighborhoods.onePointMove(pdptw.n))
      ) //onExhaustRestartAfter(simpleNeighborhoods.multipleRemoveCouples(10),5,obj)

    search.verbose = verbosity
    search.doAllMoves(obj = obj)

    search.profilingOnConsole()

    println(pdptw.toString())
    println(obj)
  }

}
