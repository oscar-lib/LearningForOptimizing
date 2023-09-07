package pdptw

import combinator.BestSlopeFirstLearningWay
import oscar.cbls._
import oscar.cbls.business.routing.display
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.visu.RoutingMapTypes

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
  // Lazily sort the relevant successors by distance
  val closestRelevantSuccessorsByDistance =
    Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantSuccessorsOfNodes)(_))


  private val simpleNeighborhoods = SimpleNeighborhoods(pdptw, oscarModel, closestRelevantPredecessorsByDistance, closestRelevantSuccessorsByDistance)


  def solve(verbosity: Int, displaySolution: Boolean, fileName: String): Unit = {
    val displayDelay = 100 //ms
    val demoDisplay =
      if(displaySolution)
        display(
          pdptw,
          oscarModel.nodePositions.map(xy => (xy._1.toDouble, xy._2.toDouble)),
          None,
          displayDelay,
          RoutingMapTypes.BasicRoutingMap,
          title = fileName)
      else null
//     val search =
//       bestSlopeFirst(
//         List(
//           simpleNeighborhoods.couplePointInsertUnroutedFirst(pdptw.n/10,best = false),
//           simpleNeighborhoods.couplePointMove(pdptw.n/10),
//           simpleNeighborhoods.onePointMove(pdptw.n/10))
//       ) onExhaustRestartAfter(simpleNeighborhoods.emptyVehicle(),2,obj)
    var search =
      new BestSlopeFirstLearningWay(
        List(
          simpleNeighborhoods.couplePointInsertUnroutedFirst(10),
          simpleNeighborhoods.couplePointInsertRoutedFirst(10),
          simpleNeighborhoods.couplePointMove(10),
          simpleNeighborhoods.onePointMove(10))
      ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v/10),5,obj)

    if(displaySolution) search = search.afterMove(demoDisplay.drawRoutes())
    search.verbose = verbosity
    search.doAllMoves(obj = obj)
    if(displaySolution) demoDisplay.drawRoutes(force = true)

    if(verbosity > 1) {
      search.profilingOnConsole()
      println(pdptw.toString())
      println(obj)
    }
    println(oscarModel.toString)
  }

}
