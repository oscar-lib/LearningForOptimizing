package tsp

import combinator.{EpsilonGreedyBanditNew, RandomCombinator}
import oscar.cbls.Objective
import oscar.cbls.business.routing.model.VRP
import util.SolverInput

import scala.concurrent.duration.Duration

case class Solver(oscarModel: Model, in: SolverInput) {

  private val tsp: VRP = oscarModel.tsp
  private val obj: Objective                             = oscarModel.objectiveFunction

  private val simpleNeighborhoods = SimpleNeighborhoods(tsp, oscarModel)

  def solve(verbosity: Int, displaySolution: Boolean, fileName: String, timeout: Int): Unit = {
    val withTimeout = timeout < Int.MaxValue
    if (displaySolution) {
      System.err.println("display not implemented yet for TSP visualisation")
    }

    val neighList = List(
      simpleNeighborhoods.insertNode(10),
      //simpleNeighborhoods.moveOneNode(10),
      //simpleNeighborhoods.twoOpt(10)
    )

    var search = in.bandit.toLowerCase() match {
      case "epsilongreedy" =>
        new EpsilonGreedyBanditNew(neighList, in) onExhaustRestartAfter(
          simpleNeighborhoods.removeNode(Math.min(50, tsp.n / 5)),
          0,
          obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15
        )
      case "random" =>
        new RandomCombinator(neighList) onExhaustRestartAfter (simpleNeighborhoods
          .removeNode(Math.min(50, tsp.n / 5)),
          0,
          obj,
          minRestarts = if (withTimeout) Int.MaxValue else 15)
    }

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

  }

}
