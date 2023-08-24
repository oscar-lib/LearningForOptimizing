import org.graalvm.compiler.core.common.CompilationIdentifier.Verbosity
import pdptw.{LiLimProblem, Model, Parser, Solver}
import scopt.OptionParser

import java.io.File
import java.nio.file.{FileSystems, Files}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object main extends App {

  abstract class Config()
  case class SolveInstanceConfig(instance: File = null, verbosity: Int = 0) extends Config()
  case class SolveSeriesConfig(seriesSize: Int = 0, verbosity: Int = 0) extends Config()
  case class SolveAllConfig(verbosity: Int = 0) extends Config()
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
          })
      )
  }

  private def solveProblem(file: File, verbosity: Int): Unit = {
    val instanceProblem: LiLimProblem = Parser(file)
    val oscarModel: Model = Model(instanceProblem)
    val solver: Solver = Solver(oscarModel)
    solver.solve(verbosity)
  }

  private val solverConfig = parser.parse(args,NoConfig()).get

  solverConfig match {
    case _: NoConfig => throw new Error("Unexpected Error, no config was found")
    case i: SolveInstanceConfig => solveProblem(i.instance, i.verbosity)

    case s: SolveSeriesConfig =>
      val dir = new File(s"examples/pdptw_${s.seriesSize}")
      val files = dir.listFiles.filter(_.isFile)
      files.foreach(x => solveProblem(x, s.verbosity))

    case a: SolveAllConfig =>
      val dir = new File(s"examples")
      val dirs = dir.listFiles
      val files = dirs.flatMap(_.listFiles.filter(_.isFile))
      files.foreach(x => solveProblem(x, a.verbosity))

  }
}
