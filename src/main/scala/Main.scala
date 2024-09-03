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

import pdptw.{LiLimProblem, Model => PDPTWModel, Parser => PDPTWParser, Solver => PDPTWSolver}
import csp.{CarSeqProblem, Model => CSPModel, Parser => CSPParser, Solver => CSPSolver}
import scopt.OptionParser

import java.io.File

import java.net.Socket
import java.io.BufferedReader
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.FileOutputStream
import bridge.NamedPipeBridge

//noinspection SpellCheckingInspection
object Main extends App {

  abstract class Config

  case class SolveInstanceConfig(
    instance: File = null,
    verbosity: Int = 0,
    problem: String = "",
    bandit: String = "bestSlopeFirst",
    display: Boolean = false,
    timeout: Int = Int.MaxValue
  ) extends Config()

  case class SolveSeriesConfig(
    seriesSize: Int = 0,
    verbosity: Int = 0,
    problem: String = "",
    bandit: String = "bestSlopeFirst",
    timeout: Int = Int.MaxValue
  ) extends Config()

  case class SolveAllConfig(
    verbosity: Int = 0,
    problem: String = "",
    bandit: String = "bestSlopeFirst",
    timeout: Int = Int.MaxValue
  ) extends Config()

  case class NoConfig() extends Config()

  private val parser = new OptionParser[Config]("LearningForOptimizing") {
    // noinspection SpellCheckingInspection
    cmd("solveInstance")
      .action((x, c) => SolveInstanceConfig())
      .text("use <solveInstance> to solve an instance of a problem")
      .children(
        opt[File]("input")
          .required()
          .text("Required: The input file containing the problem")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(instance = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("problem")
          .required()
          .abbr("p")
          .text(
            "Use this option to set the type of problem to solve:\n" +
              "    - pdptw : the pickup and delivery problem with time windows\n" +
              "    - csp   : the car sequencing problem"
          )
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(problem = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("bandit")
          .abbr("b")
          .text(
            "Use this option to set the bandit used:\n" +
              "    - bandit         : the modified bandit algorithm\n" +
              "    - epsilongreedy  : an implementation of the epsilon greedy\n" +
              "    - random         : choose neighborhoods at random\n" +
              "    - bestslopefirst : the default method"
          )
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(bandit = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("verbosity")
          .abbr("v")
          .text(
            "Use this option to set the verbosity during search :\n" +
              "    - 0 : Nothing is printed\n" +
              "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
              "    - 2 : Every accepted movement is printed\n" +
              "    - 3 : Every tried neighborhood is printed\n" +
              "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded"
          )
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(verbosity = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Unit]("display")
          .text("Display the solution resolution on a map")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(display = true)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("timeout")
          .text("Add a weak time out to the search")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(timeout = x)
              case _                         => throw new Error("Unexpected Error")
            }
          )
      )

    cmd("solveSeries")
      .action((x, c) => SolveSeriesConfig())
      .text("use <solve> to solve a problem")
      .children(
        opt[Int]("size")
          .required()
          .text(
            // todo change for csp
            "Required: The size (100,200,400,600,800,1000) of the instances to solve" +
              " (fetching all of them)"
          )
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(seriesSize = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("problem")
          .required()
          .abbr("p")
          .text(
            "Use this option to set the type of problem to solve:\n" +
              "    - pdptw : the pickup and delivery problem with time windows\n" +
              "    - csp   : the car sequencing problem"
          )
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(problem = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("bandit")
          .abbr("b")
          .text(
            "Use this option to set the bandit used:\n" +
              "    - bandit         : the modified bandit algorithm\n" +
              "    - epsilongreedy  : an implementation of the epsilon greedy\n" +
              "    - random         : choose neighborhoods at random\n" +
              "    - bestslopefirst : the default method"
          )
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(bandit = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("verbosity")
          .abbr("v")
          .text(
            "Use this option to set the verbosity during search :\n" +
              "    - 0 : Nothing is printed\n" +
              "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
              "    - 2 : Every accepted movement is printed\n" +
              "    - 3 : Every tried neighborhood is printed\n" +
              "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded"
          )
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(verbosity = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("timeout")
          .text("Add a weak time out to the search")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(timeout = x)
              case _                         => throw new Error("Unexpected Error")
            }
          )
      )

    cmd("solveAll")
      .action((x, c) => SolveAllConfig())
      .text("use <solve> to solve all PDPTW in the examples folder")
      .children(
        opt[Int]("verbosity")
          .abbr("v")
          .text(
            "Use this option to set the verbosity during search :\n" +
              "    - 0 : Nothing is printed\n" +
              "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
              "    - 2 : Every accepted movement is printed\n" +
              "    - 3 : Every tried neighborhood is printed\n" +
              "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded"
          )
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(verbosity = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("problem")
          .required()
          .abbr("p")
          .text(
            "Use this option to set the type of problem to solve:\n" +
              "    - pdptw : the pickup and delivery problem with time windows\n" +
              "    - csp   : the car sequencing problem"
          )
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(problem = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("bandit")
          .abbr("b")
          .text(
            "Use this option to set the bandit used:\n" +
              "    - bandit         : the modified bandit algorithm\n" +
              "    - epsilongreedy  : an implementation of the epsilon greedy\n" +
              "    - random         : choose neighborhoods at random\n" +
              "    - bestslopefirst : the default method"
          )
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(bandit = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("timeout")
          .text("Add a weak time out to the search")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(timeout = x)
              case _                         => throw new Error("Unexpected Error")
            }
          )
      )
  }

  private def solvePDPTW(
    file: File,
    verbosity: Int,
    bandit: String,
    display: Boolean,
    timeout: Int
  ): Unit = {
    val instanceProblem: LiLimProblem = PDPTWParser(file)
    val oscarModel: PDPTWModel        = PDPTWModel(instanceProblem)
    val solver: PDPTWSolver           = PDPTWSolver(oscarModel, bandit)
    solver.solve(verbosity, display, file.getName, timeout)
  }

  private def solveCSP(
    file: File,
    verbosity: Int,
    bandit: String,
    display: Boolean,
    timeout: Int
  ): Unit = {
    val instance: CarSeqProblem = CSPParser(file)
    val oscarModel: CSPModel    = CSPModel(instance)
    val solver: CSPSolver       = CSPSolver(oscarModel, bandit)
    solver.solve(verbosity, display, file.getName, timeout)
  }

  parser.parse(args, NoConfig()) match {
    case None =>
    case Some(solverConfig) =>
      solverConfig match {
        case _: NoConfig =>
          println("Error: No Command Given")
          println("Try --help for more information")
        case i: SolveInstanceConfig =>
          i.problem match {
            case "csp"   => solveCSP(i.instance, i.verbosity, i.bandit, i.display, i.timeout)
            case "pdptw" => solvePDPTW(i.instance, i.verbosity, i.bandit, i.display, i.timeout)
            case x       => throw new Error(s"Invalid problem name: $x")
          }

        case s: SolveSeriesConfig =>
          s.problem match {
            case "csp" =>
              val dir   = new File(s"examples/csp/csp_${s.seriesSize}")
              val files = dir.listFiles.filter(_.isFile)
              files.foreach(x => solveCSP(x, s.verbosity, s.bandit, display = false, s.timeout))
            case "pdptw" =>
              val dir   = new File(s"examples/pdptw/pdptw_${s.seriesSize}")
              val files = dir.listFiles.filter(_.isFile)
              files.foreach(x => solvePDPTW(x, s.verbosity, s.bandit, display = false, s.timeout))
            case x => throw new Error(s"Invalid problem name: $x")
          }

        case a: SolveAllConfig =>
          a.problem match {
            case "csp" =>
              val dir   = new File(s"examples/csp")
              val dirs  = dir.listFiles
              val files = dirs.flatMap(_.listFiles.filter(_.isFile))
              files.foreach(x => solveCSP(x, a.verbosity, a.bandit, display = false, a.timeout))
            case "pdptw" =>
              val dir   = new File(s"examples/pdptw")
              val dirs  = dir.listFiles
              val files = dirs.flatMap(_.listFiles.filter(_.isFile))
              files.foreach(x => solvePDPTW(x, a.verbosity, a.bandit, display = false, a.timeout))
            case x => throw new Error(s"Invalid problem name: $x")
          }
      }
  }
}
