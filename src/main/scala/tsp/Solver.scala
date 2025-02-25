package tsp

import combinator.{EpsilonGreedyBanditNew, RandomCombinator, UCBNew}
import logger.ObjectiveRecorder
import oscar.cbls.{Objective, bestSlopeFirst}
import oscar.cbls.business.routing.model.VRP
import util.SolverInput

import java.nio.file.Paths
import scala.concurrent.duration.Duration

case class Solver(oscarModel: Model, in: SolverInput) {

  private val tsp: VRP       = oscarModel.tsp
  private val obj: Objective = oscarModel.objectiveFunction

  private val simpleNeighborhoods = SimpleNeighborhoods(tsp, oscarModel)

  def solve(verbosity: Int, displaySolution: Boolean, fileName: String, timeout: Int): Unit = {
    val withTimeout = timeout < Int.MaxValue
    if (displaySolution) {
      System.err.println("display not implemented yet for TSP visualisation")
    }

    val neighList = List(
      simpleNeighborhoods.insertNode(10),
      simpleNeighborhoods.moveOneNode(10),
      simpleNeighborhoods.twoOpt(10)
    )

    var search = in.bandit.toLowerCase() match {
      case "epsilongreedy" =>
        new EpsilonGreedyBanditNew(neighList, in) onExhaustRestartAfter (
          simpleNeighborhoods.removeNode(Math.min(50, tsp.n / 5)),
          5,
          obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15
        )
      case "random" =>
        new RandomCombinator(neighList) onExhaustRestartAfter (simpleNeighborhoods.removeNode(
          Math.min(50, tsp.n / 5)
        ),
        5,
        obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
      case "ucb" =>
        new UCBNew(neighList, in) onExhaustRestartAfter (
          simpleNeighborhoods.removeNode(Math.min(50, tsp.n / 5)),
          5,
          obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15
        )
      case "bestslopefirst" =>
        bestSlopeFirst(neighList) onExhaustRestartAfter (simpleNeighborhoods.removeNode(
          Math.min(50, tsp.n / 5)
        ),
        5, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
      case _ =>
        println("warning: invalid bandit specified. Defaulting to bestSlopeFirst")
        bestSlopeFirst(neighList) onExhaustRestartAfter (simpleNeighborhoods.removeNode(
          Math.min(50, tsp.n / 5)
        ), 5, obj,
        minRestarts = if (withTimeout) Int.MaxValue else 15)
    }

    val recorder = new ObjectiveRecorder(
      oscarModel.objectiveFunction,
      _ => {
        if (tsp.unrouted.value.nonEmpty)
          None // unrouted nodes, does not correspond to a real solution
        else   // all nodes are routed, returns the length of the tour
          Some(
            oscarModel.routeLengthInvariant.value.toDouble / oscarModel.problem.multiplierFactor
          )
      }
    )
    search = search.afterMove(recorder.notifyMove())
    if (withTimeout)
      search = search.weakTimeout(Duration(timeout, "second")) saveBestAndRestoreOnExhaust obj
    search.verbose = verbosity
    search.doAllMoves(obj = obj)

    if (verbosity > 1) {
      search.profilingOnConsole()
      println(tsp.toString())
      println(obj)
    }
    println(oscarModel.toString)
    println("bestObj=" + oscarModel.objectiveFunction.value)
    val instanceName = Paths.get(fileName).getFileName.toString.stripSuffix(".xml")
    val bestKnownSolution =
      recorder.getBestKnownSolution("bks/tsp_bks.csv", instanceName).getOrElse(0.0)
    val realSolutionOverTime = recorder.realObjectiveTimeStamp
    println(f"solOverTime=" + realSolutionOverTime.map(e => f"(t:${e._1}%.3f-v:${e._2}%.3f)").mkString("[", "-", "]"))
    val integralPrimalGap = recorder.integralPrimalGap(bestKnownSolution, timeout)
    println(f"integralPrimalGap=$integralPrimalGap%.3f".replace(',','.'))
  }

}
