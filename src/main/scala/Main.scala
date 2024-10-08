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
import util.SolverInput

import java.io.File

/** Object that handles the option parsing and launches the main logic accordingly.
  */
//noinspection SpellCheckingInspection
object Main extends App {

  private sealed abstract class Config

  private case class SolveInstanceConfig(
    instance: File = null,
    display: Boolean = false,
    verbosity: Int = 0,
    problem: String = "",
    bandit: String = "bestSlopeFirst",
    timeout: Int = Int.MaxValue,
    seed: Long = 0,
    learningRate: Double = 0.1,
    slopeWeight: Double = 0.4,
    efficiencyWeight: Double = 0.2,
    moveFoundWeight: Double = 0.4,
    epsilon: Double = 0.7,
    confidence: Double = 1
  ) extends Config

  private case class SolveSeriesConfig(
    seriesSize: Int = 0,
    verbosity: Int = 0,
    problem: String = "",
    bandit: String = "bestSlopeFirst",
    timeout: Int = Int.MaxValue,
    seed: Long = 0,
    learningRate: Double = 0.1,
    slopeWeight: Double = 0.4,
    efficiencyWeight: Double = 0.2,
    moveFoundWeight: Double = 0.4,
    epsilon: Double = 0.7,
      confidence: Double = 1
  ) extends Config

  private case class SolveAllConfig(
    verbosity: Int = 0,
    problem: String = "",
    bandit: String = "bestSlopeFirst",
    timeout: Int = Int.MaxValue,
    seed: Long = 0,
    learningRate: Double = 0.1,
    slopeWeight: Double = 0.4,
    efficiencyWeight: Double = 0.2,
    moveFoundWeight: Double = 0.4,
    epsilon: Double = 0.7,
    confidence: Double = 1
  ) extends Config

  private case class NoConfig() extends Config

  private val parser = new OptionParser[Config]("lfo") {
    // noinspection SpellCheckingInspection
    head("lfo", "2.0")
    help("help").abbr("h").text("Prints this usage text")
    cmd("solveInstance")
      .action((_, _) => SolveInstanceConfig())
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
//              "    - bandit         : the modified bandit algorithm\n" +
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
              "    - 0: Nothing is printed\n" +
              "    - 1: Every seconds, an abstract of the neighborhood is printed\n" +
              "    - 2: Every accepted movement is printed\n" +
              "    - 3: Every tried neighborhood is printed\n" +
              "    - 4: Every tried value is printed (AVOID THIS if you don't want to be flooded)"
          )
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(verbosity = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Unit]("display")
          .text("Display the solution resolution on a map")
          .action((_, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(display = true)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("timeout")
          .text("Add a weak timeout to the search")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(timeout = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("learningRate")
          .abbr("lr")
          .text("Set the learning rate for the bandit algorithm (default: 0.1)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(learningRate = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("slopeWeight")
          .abbr("sw")
          .text("Set the reward weight for a neighborhood's slope (default: 0.4)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(slopeWeight = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("efficiencyWeight")
          .abbr("ew")
          .text("Set the reward weight for a neighborhood's execution time (default: 0.2)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(efficiencyWeight = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("moveFoundWeight")
          .abbr("mfw")
          .text("Set the reward weight for the neighborhood finding a valid move (default: 0.4)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(moveFoundWeight = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("epsilon")
          .abbr("e")
          .text("Set the initial value for the main parameter of epsilon-greedy (default: 0.7)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(epsilon = x)
              case _                         => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("confidence")
          .abbr("c")
          .text("Set the value for the weight of the confidence width for UCB (default: 1.0)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(confidence = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Long]("seed")
          .text("Set the random seed (currently unused)")
          .action((x, c) =>
            c match {
              case conf: SolveInstanceConfig => conf.copy(seed = x)
              case _                         => throw new Error("Unexpected Error")
            }
          )
      )

    cmd("solveSeries")
      .action((_, _) => SolveSeriesConfig())
      .text("use <solve> to solve a problem")
      .children(
        opt[Int]("size")
          .required()
          .text(
            "Required: The size of the instances to solve:\n" +
              "    - pdptw: 100, 200, 400, 600, 800, 1000\n" +
              "    - csp:   100, 200, 300, 400, 500"
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
              case conf: SolveSeriesConfig => conf.copy(problem = x)
              case _                       => throw new Error(s"Couldn't parse $x")
            }
          ),
        opt[String]("bandit")
          .abbr("b")
          .text(
            "Use this option to set the bandit used:\n" +
//              "    - bandit         : the modified bandit algorithm\n" +
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
              "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded)"
          )
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(verbosity = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Int]("timeout")
          .text("Add a weak timeout to the search")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(timeout = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("learningRate")
          .abbr("lr")
          .text("Set the learning rate for the bandit algorithm (default: 0.1)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(learningRate = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("slopeWeight")
          .abbr("sw")
          .text("Set the reward weight for a neighborhood's slope (default: 0.4)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(slopeWeight = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("efficiencyWeight")
          .abbr("ew")
          .text("Set the reward weight for a neighborhood's execution time (default: 0.2)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(efficiencyWeight = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("moveFoundWeight")
          .abbr("mfw")
          .text("Set the reward weight for the neighborhood finding a valid move (default: 0.4)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(moveFoundWeight = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("epsilon")
          .abbr("e")
          .text("Set the initial value for the main parameter of epsilon-greedy (default: 0.7)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(epsilon = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("confidence")
          .abbr("c")
          .text("Set the value for the weight of the confidence width for UCB (default: 1.0)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(confidence = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Long]("seed")
          .text("Set the random seed (currently unused)")
          .action((x, c) =>
            c match {
              case conf: SolveSeriesConfig => conf.copy(seed = x)
              case _                       => throw new Error("Unexpected Error")
            }
          )
      )

    cmd("solveAll")
      .action((_, _) => SolveAllConfig())
      .text("Solve all problem instances in the examples folder")
      .children(
        opt[Int]("verbosity")
          .abbr("v")
          .text(
            "Use this option to set the verbosity during search :\n" +
              "    - 0 : Nothing is printed\n" +
              "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
              "    - 2 : Every accepted movement is printed\n" +
              "    - 3 : Every tried neighborhood is printed\n" +
              "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded)"
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
              case conf: SolveAllConfig => conf.copy(problem = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[String]("bandit")
          .abbr("b")
          .text(
            "Use this option to set the bandit used:\n" +
//              "    - bandit         : the modified bandit algorithm\n" +
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
          .text("Add a weak timeout to the search")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(timeout = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("learningRate")
          .abbr("lr")
          .text("Set the learning rate for the bandit algorithm (default: 0.1)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(learningRate = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("slopeWeight")
          .abbr("sw")
          .text("Set the reward weight for a neighborhood's slope (default: 0.4)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(slopeWeight = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("efficiencyWeight")
          .abbr("ew")
          .text("Set the reward weight for a neighborhood's execution time (default: 0.2)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(efficiencyWeight = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("moveFoundWeight")
          .abbr("mfw")
          .text("Set the reward weight for the neighborhood finding a valid move (default: 0.4)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(moveFoundWeight = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("epsilon")
          .abbr("e")
          .text("Set the initial value for the main parameter of epsilon-greedy (default: 0.7)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(epsilon = x)
              case _                    => throw new Error("Unexpected Error")
            }
          ),
        opt[Double]("confidence")
          .abbr("c")
          .text("Set the value for the weight of the confidence width for UCB (default: 1.0)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(confidence = x)
              case _                       => throw new Error("Unexpected Error")
            }
          ),
        opt[Long]("seed")
          .text("Set the random seed (currently unused)")
          .action((x, c) =>
            c match {
              case conf: SolveAllConfig => conf.copy(seed = x)
              case _                    => throw new Error("Unexpected Error")
            }
          )
      )
  }

  private def solvePDPTW(in: SolverInput): Unit = {
    val instanceProblem: LiLimProblem = PDPTWParser(in.file)
    val oscarModel: PDPTWModel        = PDPTWModel(instanceProblem)
    val solver: PDPTWSolver           = PDPTWSolver(oscarModel, in)
    solver.solve(in.verbosity, in.display, in.file.getName, in.timeout)
  }

  private def solveCSP(in: SolverInput): Unit = {
    val instance: CarSeqProblem = CSPParser(in.file)
    val oscarModel: CSPModel    = CSPModel(instance)
    val solver: CSPSolver       = CSPSolver(oscarModel, in)
    solver.solve(in.verbosity, in.display, in.file.getName, in.timeout)
  }

  parser.parse(args, NoConfig()) match {
    case None =>
    case Some(solverConfig) =>
      solverConfig match {
        case _: NoConfig =>
          println("Error: No Command Given")
          println("Try --help for more information")

        case i: SolveInstanceConfig =>
          val in = SolverInput(
            i.instance,
            i.verbosity,
            i.bandit,
            i.display,
            i.timeout,
            i.learningRate,
            i.slopeWeight,
            i.efficiencyWeight,
            i.moveFoundWeight,
            i.epsilon,
            i.confidence
          )
          i.problem match {
            case "csp" =>
              solveCSP(in)
            case "pdptw" =>
              solvePDPTW(in)
            case x => throw new Error(s"Invalid problem name: $x")
          }

        case s: SolveSeriesConfig =>
          def checkSize(s: SolveSeriesConfig): Unit = {
            val validSizes = s.problem match {
              case "pdptw" => List(100, 200, 400, 600, 800, 1000)
              case "csp"   => List(100, 200, 300, 400, 500)
              case x       => throw new Error(s"Invalid problem name: $x")
            }
            if (!validSizes.contains(s.seriesSize)) {
              throw new Error(s"Size ${s.seriesSize} invalid for ${s.problem} setting")
            }
          }
          checkSize(s)
          s.problem match {
            case "csp" =>
              val dir   = new File(s"examples/csp/csp_${s.seriesSize}")
              val files = dir.listFiles.filter(_.isFile)
              files.foreach(x => {
                val in = SolverInput(
                  x,
                  s.verbosity,
                  s.bandit,
                  display = false,
                  s.timeout,
                  s.learningRate,
                  s.slopeWeight,
                  s.efficiencyWeight,
                  s.moveFoundWeight,
                  s.epsilon,
                  s.confidence
                )
                solveCSP(in)
              })
            case "pdptw" =>
              val dir   = new File(s"examples/pdptw/pdptw_${s.seriesSize}")
              val files = dir.listFiles.filter(_.isFile)
              files.foreach(x => {
                val in = SolverInput(
                  x,
                  s.verbosity,
                  s.bandit,
                  display = false,
                  s.timeout,
                  s.learningRate,
                  s.slopeWeight,
                  s.efficiencyWeight,
                  s.moveFoundWeight,
                  s.epsilon,
                  s.confidence
                )
                solvePDPTW(in)
              })
            case x => throw new Error(s"Invalid problem name: $x")
          }

        case a: SolveAllConfig =>
          a.problem match {
            case "csp" =>
              val dir   = new File(s"examples/csp")
              val dirs  = dir.listFiles.filter(_.isDirectory)
              val files = dirs.flatMap(_.listFiles.filter(_.isFile))
              files.foreach(x => {
                val in = SolverInput(
                  x,
                  a.verbosity,
                  a.bandit,
                  display = false,
                  a.timeout,
                  a.learningRate,
                  a.slopeWeight,
                  a.efficiencyWeight,
                  a.moveFoundWeight,
                  a.epsilon,
                  a.confidence
                )
                solveCSP(in)
              })
            case "pdptw" =>
              val dir   = new File(s"examples/pdptw")
              val dirs  = dir.listFiles.filter(_.isDirectory)
              val files = dirs.flatMap(_.listFiles.filter(_.isFile))
              files.foreach(x => {
                val in = SolverInput(
                  x,
                  a.verbosity,
                  a.bandit,
                  display = false,
                  a.timeout,
                  a.learningRate,
                  a.slopeWeight,
                  a.efficiencyWeight,
                  a.moveFoundWeight,
                  a.epsilon,
                  a.confidence
                )
                solvePDPTW(in)
              })
            case x => throw new Error(s"Invalid problem name: $x")
          }
      }
  }
}
