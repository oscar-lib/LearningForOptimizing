import pdptw.{Model, Parser, Solver}

object main extends App {
  println("Hello World!")
  val parsedFile = Parser("examples/pdptw_100/lc101.txt")
  val model = Model(parsedFile)
  val solver = new Solver(model.distanceAndTimeMatrix, model.pdpProblem, model.objectiveFunction)

  solver.solve()

}
