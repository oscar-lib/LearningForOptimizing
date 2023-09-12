package pdptw

import combinator.{BanditCombinator, BestSlopeFirstLearningWay, RandomCombinator, EpsilonGreedyBandit, NeighborhoodStatistics}
import oscar.cbls._
import oscar.cbls.business.routing.display
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.visu.RoutingMapTypes

import scala.concurrent.duration.{Duration, DurationInt}

case class Solver(oscarModel: Model, bandit: String) {
  private val distancesAndTimeMatrix: Array[Array[Long]] = oscarModel.distanceAndTimeMatrix
  private val pdptw: VRP = oscarModel.pdpProblem
  private val obj: Objective = oscarModel.objectiveFunction
  private var bestKnown: Long = obj.value

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

  def rewardFunction(neighStats: Array[NeighborhoodStatistics], nbNeigh: Int): Array[Double] = {
    println("Compute Reward")
    println(neighStats.mkString(";"))
    val objValue = obj.value
    println(s"$objValue $bestKnown")
    var totalReward = 1
//    if (objValue < bestKnown) {
//      totalReward = 1
//    } else {
//      totalReward = -1
//    }

    if (objValue < bestKnown)
      bestKnown = objValue
    var res = Array.fill(nbNeigh)(0.0)
    for (i <- 0 until nbNeigh) {
      println(s"neighbourhood outcome: $i ${neighStats(i).nbFound}")
      if (neighStats(i).nbFound > 0) {
        res(i) = res(i) + totalReward
      } else {
        //res(i) = res(i) - 0.1*totalReward
      }
    }
    println(s"reward: ${res.mkString(";")} (totalReward : $totalReward)")

    res
  }

  def solve(verbosity: Int, displaySolution: Boolean, fileName: String, timeout: Int): Unit = {
    val withTimeout = timeout < Int.MaxValue
    val displayDelay = 100 //ms
    val demoDisplay =
      if (displaySolution)
        display(
          pdptw,
          oscarModel.nodePositions.map(xy => (xy._1.toDouble, xy._2.toDouble)),
          None,
          displayDelay,
          RoutingMapTypes.BasicRoutingMap,
          title = fileName)
      else null


    val neighList = List(
      simpleNeighborhoods.couplePointInsertUnroutedFirst(10),
      simpleNeighborhoods.couplePointInsertUnroutedFirst(10,best = true),
      simpleNeighborhoods.couplePointInsertRoutedFirst(10),
      simpleNeighborhoods.couplePointInsertRoutedFirst(10,best = true),
      simpleNeighborhoods.couplePointMove(10),
      simpleNeighborhoods.couplePointMove(10, best = true),
      simpleNeighborhoods.onePointMove(10) name "1_PM_10 - first",
      simpleNeighborhoods.onePointMove(10, best = true) name "1_PM_10 - best",
      simpleNeighborhoods.doubleCouplePointMove(2),
      simpleNeighborhoods.doubleCouplePointMove(2,best = true),
      simpleNeighborhoods.oneCoupleMoveAndThenInsert(2),
      simpleNeighborhoods.oneCoupleMoveAndThenInsert(2,best = true),
      simpleNeighborhoods.segmentExchanges(pdptw.n),
      simpleNeighborhoods.segmentExchanges(pdptw.n, best = true)
    )
    var search = bandit.toLowerCase() match {
      case "bandit" => new BanditCombinator(neighList,
        simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
        if (withTimeout) Int.MaxValue else 15,
        obj,
        stats => rewardFunction(stats, neighList.length)
      ) //saveBestAndRestoreOnExhaust(obj)
      case "epsilongreedy" => new EpsilonGreedyBandit(neighList
      ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10), 1, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
      case "bestslopefirst" => bestSlopeFirst(
        neighList
      ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10), 0, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
      case "random" =>
        new RandomCombinator(neighList
        ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10), 0, obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15)
      case _ =>
        println("warning: invalid bandit specified. Defaulting to bestSlopeFirst")
        bestSlopeFirst(
          neighList
        ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10), 0, obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15)
    }

    if (displaySolution) search = search.afterMove(demoDisplay.drawRoutes()).showObjectiveFunction(oscarModel.objectiveFunction)
    if (withTimeout) search = (search.weakTimeout(Duration(timeout, "second"))) saveBestAndRestoreOnExhaust (obj)
    search.verbose = verbosity
    search.doAllMoves(obj = obj)
    if (displaySolution) demoDisplay.drawRoutes(force = true)

    if (verbosity > 1) {
      search.profilingOnConsole()
      println(pdptw.toString())
      println(obj)
    }
    println(oscarModel.toString)
  }

}
