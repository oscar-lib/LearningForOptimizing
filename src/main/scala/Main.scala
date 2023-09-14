//import org.graalvm.compiler.core.common.CompilationIdentifier.Verbosity
import pdptw.{LiLimProblem, Model, Parser, Solver}
import scopt.OptionParser

import java.io.File
import java.nio.file.{FileSystems, Files}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object Main extends App {

  abstract class Config()
  case class SolveInstanceConfig(instance: File = null,
                                 verbosity: Int = 0,
                                 bandit: String = "bestSlopeFirst",
                                 display: Boolean = false,
                                 timeout: Int = Int.MaxValue) extends Config()
  case class SolveSeriesConfig(seriesSize: Int = 0,
                               verbosity: Int = 0,
                               bandit: String = "bestSlopeFirst",
                               timeout: Int = Int.MaxValue) extends Config()
  case class SolveAllConfig(verbosity: Int = 0,
                            bandit : String = "bestSlopeFirst",
                            timeout: Int = Int.MaxValue) extends Config()
  case class NoConfig() extends Config()

  private val parser = new OptionParser[Config]("LearningForOptimizing") {
    cmd("solveInstance")
      .action((x, c) => SolveInstanceConfig())
      .text("use <solveInstance> to solve an instance of a PDPTW")
      .children(
        opt[File]("input")
          .required()
          .text("Required: The input file containing the problem")
          .action((x, c) => c match {
            case conf: SolveInstanceConfig => conf.copy(instance = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[String]("bandit")
          .abbr("b")
          .text("Use this option to set the bandit used:\n" +
            "    - bandit         : the modified bandit algorithm\n" +
            "    - epsilongreedy  : an implementation of the epsilon greedy\n" +
            "    - random         : choose neighborhoods at random\n" +
            "    - bestslopefirst : the default method")
          .action((x, c) => c match {
            case conf: SolveInstanceConfig => conf.copy(bandit = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[Int]("verbosity")
          .abbr("v")
          .text("Use this option to set the verbosity during search :\n" +
            "    - 0 : Nothing is printed\n" +
            "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
            "    - 2 : Every accepted movement is printed\n" +
            "    - 3 : Every tried neighborhood is printed\n" +
            "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded")
          .action((x, c) => c match {
            case conf: SolveInstanceConfig => conf.copy(verbosity = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[Unit]("display")
          .text("Display the solution resolution on a map")
          .action((x,c) => c match {
            case conf: SolveInstanceConfig => conf.copy(display = true)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[Int]("timeout")
          .text("Add a weak time out to the search")
          .action((x,c) => c match {
            case conf: SolveInstanceConfig => conf.copy(timeout = x)
            case _ => throw new Error("Unexpected Error")
          })
      )

    cmd("solveSeries")
      .action((x, c) => SolveSeriesConfig())
      .text("use <solve> to solve a problem")
      .children(
        opt[Int]("size")
          .required()
          .text("Required: The size (100,200,400,600,800,1000) of the instances to solve (fetching all of them)")
          .action((x, c) => c match {
            case conf: SolveSeriesConfig => conf.copy(seriesSize = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[String]("bandit")
          .abbr("b")
          .text("Use this option to set the bandit used:\n" +
            "    - bandit         : the modified bandit algorithm\n" +
            "    - epsilongreedy  : an implementation of the epsilon greedy\n" +
            "    - random         : choose neighborhoods at random\n" +
            "    - bestslopefirst : the default method")
          .action((x, c) => c match {
            case conf: SolveSeriesConfig => conf.copy(bandit = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[Int]("verbosity")
          .abbr("v")
          .text("Use this option to set the verbosity during search :\n" +
            "    - 0 : Nothing is printed\n" +
            "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
            "    - 2 : Every accepted movement is printed\n" +
            "    - 3 : Every tried neighborhood is printed\n" +
            "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded")
          .action((x, c) => c match {
            case conf: SolveSeriesConfig => conf.copy(verbosity = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[Int]("timeout")
          .text("Add a weak time out to the search")
          .action((x, c) => c match {
            case conf: SolveInstanceConfig => conf.copy(timeout = x)
            case _ => throw new Error("Unexpected Error")
          })
      )

    cmd("solveAll")
      .action((x, c) => SolveAllConfig())
      .text("use <solve> to solve all PDPTW in the examples folder")
      .children(
        opt[Int]("verbosity")
          .abbr("v")
          .text("Use this option to set the verbosity during search :\n" +
            "    - 0 : Nothing is printed\n" +
            "    - 1 : Every seconds, an abstract of the neighborhood is printed\n" +
            "    - 2 : Every accepted movement is printed\n" +
            "    - 3 : Every tried neighborhood is printed\n" +
            "    - 4 : Every tried value is printed (AVOID THIS if you don't want to be flooded")
          .action((x, c) => c match {
            case conf: SolveAllConfig => conf.copy(verbosity = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[String]("bandit")
          .abbr("b")
          .text("Use this option to set the bandit used:\n" +
            "    - bandit         : the modified bandit algorithm\n" +
            "    - epsilongreedy  : an implementation of the epsilon greedy\n" +
            "    - random         : choose neighborhoods at random\n" +
            "    - bestslopefirst : the default method")
          .action((x,c) => c match {
            case conf:SolveAllConfig => conf.copy(bandit = x)
            case _ => throw new Error("Unexpected Error")
          }),
        opt[Int]("timeout")
          .text("Add a weak time out to the search")
          .action((x, c) => c match {
            case conf: SolveInstanceConfig => conf.copy(timeout = x)
            case _ => throw new Error("Unexpected Error")
          })
      )
  }

  private def solveProblem(file: File, verbosity: Int, bandit : String, display: Boolean, timeout: Int): Unit = {
    val instanceProblem: LiLimProblem = Parser(file)
    val oscarModel: Model = Model(instanceProblem)
    val solver: Solver = Solver(oscarModel,bandit)
    solver.solve(verbosity,display,file.getName,timeout)
  }

  parser.parse(args,NoConfig()) match {
    case None =>
    case Some(solverConfig) => 
      solverConfig match {
        case _: NoConfig =>
          println("Error: No Command Given")
          println("Try --help for more information")
        case i: SolveInstanceConfig => solveProblem(i.instance, i.verbosity, i.bandit, i.display, i.timeout)

        case s: SolveSeriesConfig =>
          val dir = new File(s"examples/pdptw_${s.seriesSize}")
          val files = dir.listFiles.filter(_.isFile)
          files.foreach(x => solveProblem(x, s.verbosity,s.bandit, display = false, s.timeout))

        case a: SolveAllConfig =>
          val dir = new File(s"examples")
          val dirs = dir.listFiles
          val files = dirs.flatMap(_.listFiles.filter(_.isFile))
          files.foreach(x => solveProblem(x, a.verbosity,a.bandit, display = false, a.timeout))
      }
  }
}
