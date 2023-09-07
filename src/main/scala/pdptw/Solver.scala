package pdptw

import combinator.{BestSlopeFirstLearningWay,BanditCombinator}
import oscar.cbls._
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper

case class Solver(oscarModel: Model,bandit : Boolean) {
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
  // Lazily sort the relevant successors by distance
  val closestRelevantSuccessorsByDistance =
    Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantSuccessorsOfNodes)(_))


  private val simpleNeighborhoods = SimpleNeighborhoods(pdptw, oscarModel, closestRelevantPredecessorsByDistance, closestRelevantSuccessorsByDistance)


  def solve(verbosity: Int): Unit = {
    val neighList = List(
      simpleNeighborhoods.couplePointInsertUnroutedFirst(10),
      simpleNeighborhoods.couplePointInsertRoutedFirst(10),
      simpleNeighborhoods.couplePointMove(10),
      simpleNeighborhoods.onePointMove(10))
    val search = if (bandit) {
      new BanditCombinator(neighList,
        simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v/10),
        5)
    }
    else {
      bestSlopeFirst(
        neighList
      ) onExhaustRestartAfter(simpleNeighborhoods.emptyVehicle(),2,obj)
    }
      //   val search =
      // new BestSlopeFirstLearningWay(neighList
      // ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v/10),5,obj)

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
