package pdptw

import combinator.{BanditCombinator, BestSlopeFirstLearningWay}
import oscar.cbls._
import oscar.cbls.business.routing.display
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.visu.RoutingMapTypes
import combinator.NeighborhoodStatistics

import scala.concurrent.duration.{Duration, DurationInt}

case class Solver(oscarModel: Model,bandit : Boolean) {
  private val distancesAndTimeMatrix: Array[Array[Long]] = oscarModel.distanceAndTimeMatrix
  private val pdptw: VRP = oscarModel.pdpProblem
  private val obj: Objective = oscarModel.objectiveFunction
  private var bestKnown : Long = obj.value

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

  def rewardFunction(neighStats : Array[NeighborhoodStatistics],nbNeigh : Int) : Array[Double] = {
    println("Compute Reward")
    val objValue = obj.value
    println(s"$objValue $bestKnown")
    val totalReward = 0.5 + (bestKnown - objValue).toDouble / (2 * bestKnown)
    val totalRewardSig = 1/(1 + Math.exp(-5 * (totalReward - 0.5)))
    if (objValue < bestKnown)
      bestKnown = objValue
    val totalGain = neighStats.map(_.totalGain).sum
    val res = Array.tabulate(nbNeigh)(i => {
      if (objValue < bestKnown)
        neighStats(i).totalGain.toDouble * totalRewardSig / totalGain
      else
        (totalGain - neighStats(i).totalGain).toDouble * totalRewardSig/totalGain
    })
    val totalRes = res.sum
    for (i <- 0 until nbNeigh)
      res(i) = res(i)/totalRes
    println(s"reward: ${res.mkString(";")} (totalReward : $totalReward - $totalRewardSig)")

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
      simpleNeighborhoods.couplePointInsertRoutedFirst(10),
      simpleNeighborhoods.couplePointMove(10),
      simpleNeighborhoods.onePointMove(10))
    var search = if (bandit) {
      new BanditCombinator(neighList,
        simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v/10),
        if(withTimeout) Int.MaxValue else 15,
        stats => rewardFunction(stats,neighList.length)
      )
    }
    else {
      bestSlopeFirst(
        neighList
      ) onExhaustRestartAfter (simpleNeighborhoods.emptyVehicle(),0,obj,
        minRestarts = if(withTimeout)Int.MaxValue else 15)
      // new BestSlopeFirstLearningWay(neighList
      // ) onExhaustRestartAfter(simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v/10),10,obj)

    }

    if(displaySolution) search = search.afterMove(demoDisplay.drawRoutes()).showObjectiveFunction(oscarModel.objectiveFunction)
    if(withTimeout) search = search.weakTimeout(Duration(timeout,"second"))
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
