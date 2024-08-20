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

import combinator._
import oscar.cbls._
import oscar.cbls.core.search.Neighborhood

import scala.concurrent.duration.Duration

case class Solver(oscarModel: Model, bandit: String) {

  private val obj: Objective = oscarModel.obj

  private val simpleNeighborhoods = SimpleNeighborhoods(oscarModel)

  def solve(verbosity: Int, display: Boolean, fileName: String, timeout: Int): Unit = {

    val withTimeout = timeout < Int.MaxValue

    val neighList: List[Neighborhood] = List(
      simpleNeighborhoods.swapsNeighborhood(),
      simpleNeighborhoods.shuffleNeighborhood(4)
    )

    val banditNeighborhood: Neighborhood = bandit.toLowerCase() match {
      case "bandit" => BanditCombinator(neighList, ???, 0, obj, ???) saveBestAndRestoreOnExhaust obj

      case "banditaftermove" =>
        BanditCombinator(neighList, ???, 0, obj, ???) saveBestAndRestoreOnExhaust obj

      case "banditrollingaverage" =>
        BanditCombinator(neighList, ???, 0, obj, ???) saveBestAndRestoreOnExhaust obj

      case "epsilongreedy" => new EpsilonGreedyBandit(neighList)

      case "epsilongreedynew" => new EpsilonGreedyBanditNew(neighList)

      case "ucbnew" => new UCB1(neighList)

      case "bestslopefirst" => bestSlopeFirst(neighList)

      case "bestsslopefirstnew" => new BestSlopeFirstNew(neighList)

      case "random" => new RandomCombinator(neighList)

      case _ =>
        println("warning: invalid bandit specified. Defaulting to bestSlopeFirst")
        bestSlopeFirst(neighList)
    }

    var search = {
      banditNeighborhood match {
        case BanditCombinator(_, _, _, _, _, _, _, _) => banditNeighborhood
        case _ =>
          banditNeighborhood onExhaustRestartAfter (???, 0, obj, minRestarts =
            if (withTimeout) Int.MaxValue else 15)
      }
    }

    val c = oscarModel.constraintSystem
    if (withTimeout) {
      search = search.weakTimeout(Duration(timeout, "second")) saveBestAndRestoreOnExhaust obj
    }
    search.verbose = verbosity
    search.doAllMoves(_ => c.isTrue, obj = obj)
    if (verbosity > 1) {
      search.profilingOnConsole()
      println(obj)
    }
    println(oscarModel)

    println("car sequence:" + oscarModel.carSequence.map(_.value).mkString(","))
    println("bestObj=" + oscarModel.obj.value)
    println(
      if (c.violation.value == 0) "Problem solved"
      else s"PROBLEM COULD NOT BE SOLVED: ${c.violation}"
    )
  }
}
