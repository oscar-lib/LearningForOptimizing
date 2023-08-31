package pdptw

import oscar.cbls._
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper

case class Solver(oscarModel: Model) {
  private val distancesAndTimeMatrix: Array[Array[Long]] = oscarModel.distanceAndTimeMatrix
  private val pdptw: VRP = oscarModel.pdpProblem
  private val obj: Objective = oscarModel.objectiveFunction

  // Relevant predecessors definition for each node (here any node can be the predecessor of another node)
  val relevantPredecessorsOfNodes =
    TransferFunction.relevantPredecessorsOfNodes(pdptw.n, pdptw.v, oscarModel.timeWindows, distancesAndTimeMatrix)
  // Lazily sort the relevant predecessors by distance
  val closestRelevantPredecessorsByDistance =
    Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantPredecessorsOfNodes)(_))
  // Relevant predecessors definition for each node (here any node can be the predecessor of another node)
  val relevantSuccessorsOfNodes =
    TransferFunction.relevantSuccessorsOfNodes(pdptw.n, pdptw.v, oscarModel.timeWindows, distancesAndTimeMatrix)
  val closestRelevantSuccessorsByDistance =
    Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantSuccessorsOfNodes)(_))


  private val simpleNeighborhoods = SimpleNeighborhoods(pdptw, oscarModel, closestRelevantPredecessorsByDistance, closestRelevantSuccessorsByDistance)


  def solve(verbosity: Int): Unit = {
    val search =
      //new BestSlopeFirstLearningWay(
      bestSlopeFirst(
        List(
          simpleNeighborhoods.couplePointInsertUnroutedFirst(20),
          simpleNeighborhoods.couplePointInsertRoutedFirst(20),
          simpleNeighborhoods.couplePointMove(20),
          simpleNeighborhoods.onePointMove(20))
      ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v/10),5,obj)

    search.verbose = verbosity
    search.doAllMoves(obj = obj)

    if(verbosity > 1) {
      search.profilingOnConsole()
      println(pdptw.toString())
      println(obj)
    }
    println(oscarModel.toString)
  }

}
