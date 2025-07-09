package tsp

import combinator.{BanditSelector, EpsilonGreedyBanditNew, RandomCombinator, UCBNew}
import logger.{MoveRecorder, ObjectiveRecorder}
import oscar.cbls.{Objective, bestSlopeFirst, roundRobin}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.Neighborhood
import util.SolverInput

import java.nio.file.Paths
import scala.concurrent.duration.Duration

case class Solver(oscarModel: Model, in: SolverInput) {

  private val tsp: VRP       = oscarModel.tsp // problem to solve
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

    // set the bandit according to the user input
    var search : Neighborhood = in.bandit.toLowerCase() match {
      case "epsilongreedy" =>
        new EpsilonGreedyBanditNew(neighList, in)
      case "random" =>
        new RandomCombinator(neighList)
      case "ucb" =>
        new UCBNew(neighList, in)
      case "bestslopefirst" =>
        bestSlopeFirst(neighList)
      case "roundrobin" =>
        roundRobin(neighList.zip((0 to neighList.length).map(i => 1)))
      case _ =>
        println("warning: invalid bandit specified. Defaulting to bestSlopeFirst")
        bestSlopeFirst(neighList)
    }

    val history = new MoveRecorder()
    if (in.printHistory) {
      search match {
        case b: BanditSelector => {
          b.addResetCallBack(() => history.notifyReset())
          b.addMoveCallBack((neigh, result) => history.notifySearchResult(neigh, result))
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
          None // unrouted nodes, does not correspond to a real solution
        else   // all nodes are routed, returns the length of the tour
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
    val instanceName = Paths.get(fileName).getFileName.toString.stripSuffix(".xml").stripSuffix(".tsp")
    val bestKnownSolution =
      recorder.getBestKnownSolution("bks/tsp_bks.csv", instanceName).getOrElse(0.0)
    val realSolutionOverTime = recorder.realObjectiveTimeStamp
    println(f"solOverTime=" + realSolutionOverTime.map(e => f"(t:${e._1}%.3f-v:${e._2}%.3f)").mkString("[", "-", "]"))
    val integralPrimalGap = recorder.integralPrimalGap(bestKnownSolution, timeout)
    println(f"integralPrimalGap=$integralPrimalGap%.3f".replace(',','.'))
    println(f"history=" + history.toString)
  }

}
