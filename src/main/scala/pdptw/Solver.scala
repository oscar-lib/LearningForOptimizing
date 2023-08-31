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
    TransferFunction.relevantPredecessorsOfNodes(pdptw.n, pdptw.v, oscarModel.timeWindows, distancesAndTimeMatrix)
  // Sort them lazily by distance
  val closestRelevantNeighborsByDistance =
    Array.tabulate(pdptw.n)(DistanceHelper.lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantPredecessorsOfNodes)(_))

  private val simpleNeighborhoods = SimpleNeighborhoods(pdptw, oscarModel, closestRelevantNeighborsByDistance)


  def solve(verbosity: Int): Unit = {

    val bestSolution = List(List(81, 78, 104, 76, 71, 70, 73, 77, 79, 80),
      List(57, 55, 54, 53, 56, 58, 60, 59),
      List(98, 96, 95, 94, 92, 93, 97, 106, 100, 99),
      List(13, 17, 18, 19, 15, 16, 14, 12),
      List(32, 33, 31, 35, 37, 38, 39, 36, 105, 34),
      List(90, 87, 86, 83, 82, 84, 85, 88, 89, 91),
      List(43, 42, 41, 40, 44, 46, 45, 48, 51, 101, 50, 52, 49, 47),
      List(67, 65, 63, 62, 74, 72, 61, 64, 102, 68, 66, 69),
      List(5, 3, 7, 8, 10, 11, 9, 6, 4, 2, 1, 75),
      List(20, 24, 25, 27, 29, 30, 28, 26, 23, 103, 22, 21))

    val bestSolutionInOscarWay =
      List.tabulate(pdptw.v)(vehicle => {
        if(vehicle < bestSolution.size) List(vehicle) ++ bestSolution(vehicle).map(x => x+pdptw.v-1)
        else List(vehicle)
      }).flatten

//    pdptw.setCircuit(bestSolutionInOscarWay)
    println(obj)

    val search =
      //new BestSlopeFirstLearningWay(
      bestSlopeFirst(
        List(
          simpleNeighborhoods.couplePointInsertUnroutedFirst(pdptw.n/10,best = false),
          simpleNeighborhoods.couplePointMove(pdptw.n/10),
          simpleNeighborhoods.onePointMove(pdptw.n/10))
      ) //onExhaustRestartAfter(simpleNeighborhoods.emptyVehicle(),10,obj)

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
