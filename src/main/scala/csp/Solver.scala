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

package csp

import util.SolverInput
import combinator._
import logger.ObjectiveRecorder
import oscar.cbls._
import oscar.cbls.core.search.Neighborhood

import java.nio.file.Paths
import scala.concurrent.duration.Duration

/** This class is responsible for the handling of the local search procedure for the given car
  * sequencing problem; in particular, it regulates its behavior depending on the selected bandit
  * algorithm.
  *
  * @param cspModel
  *   the model of the given CSP instance
  * @param in
  *   the remaining input data
  */
case class Solver(cspModel: Model, in: SolverInput) {
  private val obj = cspModel.obj
  private val sn  = SimpleNeighborhoods(cspModel)

  def solve(verbosity: Int, display: Boolean, fileName: String, timeout: Int): Unit = {
    val withTimeout = timeout < Int.MaxValue
    val neighList: List[Neighborhood] =
      List(
        sn.wideningSwapMostViolated(),
        sn.swapMostViolated(),
        sn.wideningFlipMostViolated(),
        sn.wideningSwap(),
        sn.wideningFlip(),
        sn.swap(),
        sn.oneCarMove(),
        sn.oneCarMoveMostViolated()
      )
    val rewardModel = in.rewardType.toLowerCase() match {
      case "r1" =>
        new OriginalRewardModel(
          wSol = in.moveFoundWeight,
          wEff = in.efficiencyWeight,
          wSlope = in.slopeWeight
        )
      case "r2" => new Gain()
      case "r3" => new LogGain()
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown reward type: ${in.rewardType}. Supported types are: r1, r2, r3."
        )
    }

    val mostViolated = cspModel.mostViolatedCars
    val violated     = cspModel.violatedCars
    val restart1: Neighborhood =
      sn.shuffle(indices = Some(mostViolated)) guard (() => mostViolated.value.size > 2)

    val restart2: Neighborhood =
      sn.shuffle(indices = Some(violated), numOfPositions = Some(5 max violated.value.size / 2))

    val restart3: Neighborhood = sn.shuffle(numOfPositions = Some(cspModel.instance.nCars / 2))
    val restart4: Neighborhood = sn.shuffle()

    val banditNeighborhood: Neighborhood = in.bandit.toLowerCase() match {
      case "epsilongreedy" => new EpsilonGreedyBanditNew(neighList, in, rewardModel)
      case "dqn" =>
        new StatefulCombinator(
          neighList,
          cspModel,
          lr = in.learningRate,
          batchSize = in.batchSize,
          epsilon = in.epsilon,
          clipping = in.clipping,
          ddqn = in.ddqn,
          debug = in.debug,
          algo = RLAlgorithm.DQN,
          device = in.device,
          objective = obj,
          acceptanceCriterion = in.acceptanceCriterion,
          loadFrom = in.loadFrom,
          saveTo = in.saveTo,
          training = in.training,
          rewardModel = rewardModel
        )
      case "ucb"            => new UCBNew(neighList, in, rewardModel)
      case "bestslopefirst" => bestSlopeFirst(neighList)
      case "random"         => new RandomCombinator(neighList)
      case "roundrobin" =>
        roundRobin(neighList.zip((0 to neighList.length).map(i => 1)))
      case _ =>
        throw new IllegalArgumentException(s"Unknown bandit type: ${in.bandit}.")
    }

    var search: Neighborhood = {
      banditNeighborhood match {
        case BanditCombinator(_, _, _, _, _, _, _, _) => banditNeighborhood
        case _ =>
          banditNeighborhood
            .onExhaustRestartAfter(
              restart1.acceptAll(),
              5,
              obj,
              minRestarts = if (withTimeout) Int.MaxValue else 5
            )
            .onExhaustRestartAfter(
              restart2.acceptAll(),
              5,
              obj,
              minRestarts = if (withTimeout) Int.MaxValue else 5
            )
            .onExhaustRestartAfter(
              restart3.acceptAll(),
              5,
              obj,
              minRestarts = if (withTimeout) Int.MaxValue else 5
            )
      }
    }

    val c = cspModel.constraintSystem
    if (withTimeout) {
      search = search.weakTimeout(Duration(timeout, "second")) saveBestAndRestoreOnExhaust obj
    }
    if (display)
      search = search showObjectiveFunction obj

    val recorder = new ObjectiveRecorder(cspModel.obj, _ => Some(cspModel.obj.value.toDouble))
    search = search.afterMove(recorder.notifyMove())

    search.verbose = verbosity
    search.doAllMoves(_ => c.isTrue, obj = obj)
    if (verbosity >= 1) {
      search.profilingOnConsole()
      println(obj)
    }
    println(cspModel)

    println("car sequence:" + cspModel.carSequence.map(_.value).mkString(","))
    println("bestObj=" + cspModel.obj.value)
    println(
      if (c.violation.value == 0) "Problem solved"
      else s"PROBLEM COULD NOT BE SOLVED: ${c.violation}"
    )
    val instanceName         = Paths.get(fileName).getFileName.toString
    val realSolutionOverTime = recorder.realObjectiveTimeStamp
    println(
      f"solOverTime=" + realSolutionOverTime
        .map(e => f"(t:${e._1}%.3f-step:${e._2}-v:${e._3})")
        .mkString("[", "-", "]")
    )
    val currentDirectory = System.getProperty("user.dir")
    val rootDir          = currentDirectory.split("LearningForOptimizing")(0)
    val bestKnownSolution =
      recorder
        .getBestKnownSolution(rootDir + "/LearningForOptimizing/bks/csp_bks.csv", instanceName)
        .getOrElse(0.0)
    val integralPrimalGap = recorder.integralPrimalGap(bestKnownSolution, timeout)
    println(f"integralPrimalGap=$integralPrimalGap%.3f")

    if (banditNeighborhood.isInstanceOf[StatefulCombinator]) {
      banditNeighborhood.asInstanceOf[StatefulCombinator].close();
    }
  }
}
