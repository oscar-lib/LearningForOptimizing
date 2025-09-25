package tsp

import combinator.{
  BanditSelector,
  BestSlopeFirstNew,
  EpsilonGreedyBanditNew,
  RandomCombinator,
  RandomSelector,
  RoundRobinSelector,
  UCBNew
}
import logger.{MoveRecorder, ObjectiveRecorder, WeightRecorder}
import oscar.cbls.{bestSlopeFirst, roundRobin, Objective}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.Neighborhood
import util.SolverInput

import java.nio.file.Paths
import scala.concurrent.duration.Duration
import combinator.OriginalRewardModel
import combinator.Gain
import bridge.MessageType.REWARD
import oscar.cbls.business.routing.display
import combinator.LogGain
import combinator.StatefulCombinator
import combinator.RLAlgorithm

case class Solver(oscarModel: Model, in: SolverInput) {

  private val tsp: VRP       = oscarModel.tsp               // problem to solve
  private val obj: Objective = oscarModel.objectiveFunction // corresponding objective function

  // neighborhoods suited for optimizing the problem
  private val simpleNeighborhoods = SimpleNeighborhoods(tsp, oscarModel)

  def solve(verbosity: Int, displaySolution: Boolean, fileName: String, timeout: Int): Unit = {
    val withTimeout = timeout < Int.MaxValue
    if (displaySolution) {
      System.err.println("display not implemented yet for TSP visualisation")
    }

    // small list of neighborhoods usable for the problem
    val neighList = List(
      simpleNeighborhoods.insertNode(10),
      simpleNeighborhoods.moveOneNode(10),
      simpleNeighborhoods.twoOpt(10)
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
    }

    // set the bandit according to the user input
    var search: Neighborhood = in.bandit.toLowerCase() match {
      case "epsilongreedy" =>
        new EpsilonGreedyBanditNew(neighList, in)
      case "random" =>
        // new RandomCombinator(neighList)
        new RandomSelector(neighList)
      case "ucb" =>
        new UCBNew(neighList, in)
      case "bestslopefirst" =>
        bestSlopeFirst(neighList)
      // new BestSlopeFirstNew(neighList)
      case "roundrobin" =>
        // roundRobin(neighList.zip((0 to neighList.length).map(i => 1)))
        new RoundRobinSelector(neighList)
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown reward type: ${in.rewardType}. Supported types are: r1, r2, r3."
        )
    }
    // set the bandit according to the user input
    var search: Neighborhood = in.bandit.toLowerCase() match {
      case "epsilongreedy"  => new EpsilonGreedyBanditNew(neighList, in, rewardModel)
      case "random"         => new RandomCombinator(neighList)
      case "ucb"            => new UCBNew(neighList, in, rewardModel)
      case "bestslopefirst" => bestSlopeFirst(neighList)
      case "roundrobin"     => roundRobin(neighList.zip((0 to neighList.length).map(i => 1)))
      case "dqn" =>
        new StatefulCombinator(
          neighList,
          oscarModel,
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
      case _ => throw new IllegalArgumentException(s"Unknown bandit type: ${in.bandit}.")
    }

    val history       = new MoveRecorder(obj)
    val weightHistory = new WeightRecorder(null)
    if (in.printHistory) {
      search match {
        case b: BanditSelector => {
          weightHistory.setBanditSelector(b)
          b.addResetCallBack(() => history.notifyReset())
          b.addMoveCallBack((neigh, result) => history.notifySearchResult(neigh, result))
          b.addResetCallBack(() => weightHistory.registerWeights())
          b.addMoveCallBack((_, _) => weightHistory.registerWeights())
        }
        case _ => // do nothing
      }
    }

    search = search onExhaustRestartAfter (
      simpleNeighborhoods.removeNode(Math.min(50, tsp.n / 5)),
      5,
      obj,
      minRestarts = if (withTimeout) Int.MaxValue else 15
    )

    // tracks the objective evolution over time
    val recorder = new ObjectiveRecorder(
      oscarModel.objectiveFunction,
      _ => {
        if (tsp.unrouted.value.nonEmpty)
          None  // unrouted nodes, does not correspond to a real solution
        else    // all nodes are routed, returns the length of the tour
          Some( // divide by multiplier factor to get back the original double values
            oscarModel.routeLengthInvariant.value.toDouble / oscarModel.problem.multiplierFactor
          )
      }
    )
    search = search.afterMove(recorder.notifyMove())
    if (withTimeout)
      search = search.weakTimeout(Duration(timeout, "second")) saveBestAndRestoreOnExhaust obj
    search.verbose = verbosity
    search.doAllMoves(obj = obj)

    // search finished, print solution reached
    if (verbosity > 1) {
      search.profilingOnConsole()
      println(tsp.toString())
      println(obj)
    }
    println(oscarModel.toString)
    println("bestObj=" + oscarModel.objectiveFunction.value)
    // retrieve the best known solution and compute the gap over time compared to it
    val instanceName =
      Paths.get(fileName).getFileName.toString.stripSuffix(".xml").stripSuffix(".tsp")
    val currentDirectory = System.getProperty("user.dir")
    val rootDir          = currentDirectory.split("LearningForOptimizing")(0)
    val bestKnownSolution =
      recorder
        .getBestKnownSolution(rootDir + "/LearningForOptimizing/bks/tsp_bks.csv", instanceName)
        .getOrElse(0.0)
    val realSolutionOverTime = recorder.realObjectiveTimeStamp
    println(
      f"solOverTime=" + realSolutionOverTime
        .map(e => f"(t:${e._1}%.3f-t:${e._2}-v:${e._3})")
        .mkString("[", "-", "]")
    )
    val integralPrimalGap = recorder.integralPrimalGap(bestKnownSolution, timeout)
    println(f"integralPrimalGap=$integralPrimalGap%.3f".replace(',', '.'))
    println(f"history=" + history.toString)
    if (search.isInstanceOf[StatefulCombinator]) {
      search.asInstanceOf[StatefulCombinator].close();
    }
    if (weightHistory.banditSelector != null)
      println(weightHistory.toString)
  }

}
