// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package pdptw

import util.SolverInput

import combinator._
import logger.ObjectiveRecorder
import oscar.cbls._
import oscar.cbls.business.routing.display
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.visu.RoutingMapTypes

import java.nio.file.Paths
import scala.concurrent.duration.Duration

/** This class is responsible for the handling of the local search procedure for the given pickup
  * and delivery problem with time windows; in particular, it regulates its behavior depending on
  * the selected bandit algorithm.
  *
  * @param oscarModel
  *   the model of the given PDPTW instance
  * @param in
  *   the remaining input data
  */
case class Solver(oscarModel: Model, in: SolverInput) {
  private val distancesAndTimeMatrix: Array[Array[Long]] = oscarModel.distanceAndTimeMatrix
  private val pdptw: VRP                                 = oscarModel.pdpProblem
  private val obj: Objective                             = oscarModel.objectiveFunction
  private var bestKnown: Long                            = obj.value

  // Relevant predecessors definition for each node (here any node can be the predecessor of another node)
  val relevantPredecessorsOfNodes: Map[Int, Iterable[Int]] =
    TransferFunction.relevantPredecessorsOfNodes(
      pdptw.n,
      pdptw.v,
      oscarModel.timeWindows,
      distancesAndTimeMatrix
    )
  // Lazily sort the relevant predecessors by distance
  val closestRelevantPredecessorsByDistance: Array[Iterable[Int]] =
    Array.tabulate(pdptw.n)(
      DistanceHelper
        .lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantPredecessorsOfNodes)(_)
    )
  // Relevant predecessors definition for each node (here any node can be the predecessor of another node)
  val relevantSuccessorsOfNodes: Map[Int, Iterable[Int]] =
    TransferFunction.relevantSuccessorsOfNodes(
      pdptw.n,
      pdptw.v,
      oscarModel.timeWindows,
      distancesAndTimeMatrix
    )
  // Lazily sort the relevant successors by distance
  val closestRelevantSuccessorsByDistance: Array[Iterable[Int]] =
    Array.tabulate(pdptw.n)(
      DistanceHelper
        .lazyClosestPredecessorsOfNode(distancesAndTimeMatrix, relevantSuccessorsOfNodes)(_)
    )

  private val simpleNeighborhoods = SimpleNeighborhoods(
    pdptw,
    oscarModel,
    closestRelevantPredecessorsByDistance,
    closestRelevantSuccessorsByDistance
  )

  def rewardFunction(neighStats: Array[NeighborhoodStatistics], nbNeigh: Int): Array[Double] = {
    // println("Compute Reward")
    // println(neighStats.mkString(";"))
    val objValue = obj.value
    // println(s"$objValue $bestKnown")
    val totalReward    = 0.5 + (bestKnown - objValue).toDouble / (2 * bestKnown)
    val totalRewardSig = 1 / (1 + Math.exp(-5 * (totalReward - 0.5)))
    val totalGain      = neighStats.map(_.totalGain).sum
    val res = Array.tabulate(nbNeigh)(i => {
      if (objValue < bestKnown) neighStats(i).totalGain.toDouble * totalRewardSig / totalGain
      else (totalGain - neighStats(i).totalGain).toDouble * totalRewardSig / totalGain
    })
    val totalRes = res.sum
    if (objValue < bestKnown)
      bestKnown = objValue
    for (i <- 0 until nbNeigh)
      res(i) = res(i) / totalRes
    // println(s"reward: ${res.mkString(";")} (totalReward : $totalReward - $totalRewardSig)")

    res
  }

  def rewardFunctionAfterMove(
    neighStats: Array[NeighborhoodStatistics],
    nbNeigh: Int
  ): Array[Double] = {
//    println("Compute Reward")
//    println(neighStats.mkString(";"))
    val objValue = obj.value
//    println(s"$objValue $bestKnown")
    val totalReward = 1
//    if (objValue < bestKnown) {
//      totalReward = 1
//    } else {
//      totalReward = -1
//    }

    if (objValue < bestKnown)
      bestKnown = objValue
    val res = Array.fill(nbNeigh)(0.0)
    for (i <- 0 until nbNeigh) {
//      println(s"neighbourhood outcome: $i ${neighStats(i).nbFound}")
      if (neighStats(i).nbFound > 0) {
        res(i) = res(i) + totalReward
      } else if (neighStats(i).nbNotFound > 0) {
        res(i) = res(i) - 0.4 * totalReward
      }
    }

//    var foundBetter = 0
//    var foundWorse = 0
//    for (i <- 0 until nbNeigh) {
//      if (neighStats(i).nbFound > 0) {
//        foundBetter = 1
//      }
//      if (neighStats(i).nbNotFound > 0) {
//        foundWorse = 1
//      }
//    }
//    var tr : Double = 0.0
//    if (foundBetter == 1) tr = totalReward
//    else if (foundWorse == 1) tr = -0.3 * totalReward
//    for (i <- 0 until nbNeigh) {
//      println(s"neighbourhood outcome: $i ${neighStats(i).nbFound}")
//      if (neighStats(i).nbFound > 0 || neighStats(i).nbNotFound > 0) {
//        res(i) = res(i) + tr
//      }
//    }
//    println(s"reward: ${res.mkString(";")} (totalReward : $totalReward)")

    res
  }

  // noinspection SpellCheckingInspection
  def solve(verbosity: Int, displaySolution: Boolean, fileName: String, timeout: Int): Unit = {
    val withTimeout  = timeout < Int.MaxValue
    val displayDelay = 100 // ms
    val demoDisplay =
      if (displaySolution)
        display(
          pdptw,
          oscarModel.nodePositions.map(xy => (xy._1.toDouble, xy._2.toDouble)),
          None,
          displayDelay,
          RoutingMapTypes.BasicRoutingMap,
          title = fileName
        )
      else null

    val neighList = List(
      simpleNeighborhoods.couplePointInsertUnroutedFirst(10),
//      simpleNeighborhoods.couplePointInsertUnroutedFirst(10,best = true),
      simpleNeighborhoods.couplePointInsertRoutedFirst(10),
//      simpleNeighborhoods.couplePointInsertRoutedFirst(10,best = true),
      simpleNeighborhoods.couplePointMove(10),
//      simpleNeighborhoods.couplePointMove(10, best = true),
      simpleNeighborhoods.onePointMove(10) name "1_PM_10 - first",
//      simpleNeighborhoods.onePointMove(10, best = true) name "1_PM_10 - best",
//      simpleNeighborhoods.doubleCouplePointMove(2),
//      simpleNeighborhoods.doubleCouplePointMove(2,best = true),
      simpleNeighborhoods.oneCoupleMoveAndThenInsert(2),
//      simpleNeighborhoods.oneCoupleMoveAndThenInsert(2,best = true),
      simpleNeighborhoods.segmentExchanges(pdptw.n)
//      simpleNeighborhoods.segmentExchanges(pdptw.n, best = true)
    )
    var search = in.bandit.toLowerCase() match {
//      case "bandit" =>
//        BanditCombinator(
//          neighList,
//          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
//          if (withTimeout) Int.MaxValue else 15,
//          obj,
//          stats => rewardFunction(stats, neighList.length)
//        ) saveBestAndRestoreOnExhaust obj
//      case "banditaftermove" =>
//        BanditCombinator(
//          neighList,
//          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
//          if (withTimeout) Int.MaxValue else 15,
//          obj,
//          stats => rewardFunctionAfterMove(stats, neighList.length),
//          afterMove = true
//        ) saveBestAndRestoreOnExhaust obj
//      case "banditrollingaverage" =>
//        BanditCombinator(
//          neighList,
//          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
//          if (withTimeout) Int.MaxValue else 15,
//          obj,
//          stats => rewardFunction(stats, neighList.length),
//          rollingAverage = true
//        ) saveBestAndRestoreOnExhaust obj
//      case "epsilongreedy" =>
//        new EpsilonGreedyBandit(neighList) onExhaustRestartAfter (
//          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
//          0,
//          obj,
//          minRestarts = if (withTimeout) Int.MaxValue else 15
//        )
      case "epsilongreedy" =>
        new EpsilonGreedyBanditNew(neighList, in) onExhaustRestartAfter (
          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
          0,
          obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15
        )
//      case "ucb1" =>
//        new UCB1(neighList) onExhaustRestartAfter (
//          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
//          0,
//          obj,
//          minRestarts = if (withTimeout) Int.MaxValue else 15
//        )
      case "ucb" =>
        new UCBNew(neighList, in) onExhaustRestartAfter (
          simpleNeighborhoods.emptyMultiplesVehicle(pdptw.v / 10),
          0,
          obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15
        )
      case "bestslopefirst" =>
//        println("Using bestSlopeFirst")
        bestSlopeFirst(neighList) onExhaustRestartAfter (simpleNeighborhoods.emptyMultiplesVehicle(
          pdptw.v / 10
        ), 0, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
//      case "bestslopefirstnew" =>
//        new BestSlopeFirstNew(neighList) onExhaustRestartAfter (simpleNeighborhoods
//          .emptyMultiplesVehicle(pdptw.v / 10), 0, obj,
//        minRestarts = if (withTimeout) Int.MaxValue else 15)
      case "random" =>
        new RandomCombinator(neighList) onExhaustRestartAfter (simpleNeighborhoods
          .emptyMultiplesVehicle(pdptw.v / 10), 0, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)

      case _ =>
        println("warning: invalid bandit specified. Defaulting to bestSlopeFirst")
        bestSlopeFirst(neighList) onExhaustRestartAfter (simpleNeighborhoods.emptyMultiplesVehicle(
          pdptw.v / 10
        ), 0, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
    }

    val recorder = new ObjectiveRecorder(
      oscarModel.objectiveFunction,
      _ => {
        if (pdptw.unrouted.value.nonEmpty)
          None // unrouted nodes, does not correspond to a real solution
        else   // all nodes are routed, returns the length of the tour
          Some(
            oscarModel.routeLengthsInvariant
              .map(_.value)
              .sum
              .toDouble / oscarModel.liLimProblem.multiplierFactor
          )
      }
    )
    search = search.afterMove(recorder.notifyMove())
    if (displaySolution)
      search = search
        .afterMove(demoDisplay.drawRoutes())
        .showObjectiveFunction(oscarModel.objectiveFunction)
    if (withTimeout)
      search = search.weakTimeout(Duration(timeout, "second")) saveBestAndRestoreOnExhaust obj
    search.verbose = verbosity
    search.doAllMoves(obj = obj)
    if (displaySolution) demoDisplay.drawRoutes(force = true)

    if (verbosity > 1) {
      search.profilingOnConsole()
      println(pdptw.toString())
      println(obj)
    }
    println(oscarModel.toString)
    println("bestObj=" + oscarModel.objectiveFunction.value)
    //println(recorder)
    val instanceName = Paths.get(fileName).getFileName.toString
    val bestKnownSolution =
      recorder.getBestKnownSolution("bks/pdptw_bks.csv", instanceName).getOrElse(0.0)
    //val gapOverTime = recorder.primalGapOverTime(bestKnownSolution, timeout)
    //println(f"primalGapOverTime=" + gapOverTime.map(e => f"(t=${e._1}%.3f-v=${e._2}%.6f)").mkString("[", "-", "]"))
    val realSolutionOverTime = recorder.realObjectiveTimeStamp
    println(f"solOverTime=" + realSolutionOverTime.map(e => f"(t:${e._1}%.3f-v:${e._2}%.3f)").mkString("[", "-", "]"))
    val integralPrimalGap = recorder.integralPrimalGap(bestKnownSolution, timeout)
    println(f"integralPrimalGap=$integralPrimalGap%.3f".replace(',','.'))
  }
}
