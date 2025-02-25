package tsp

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.lib.search.neighborhoods.SwapsNeighborhood
import util.SolverInput

case class Solver(oscarModel: Model, in: SolverInput) {

  private val tsp: VRP = oscarModel.tsp

  private val simpleNeighborhoods = SimpleNeighborhoods(tsp, oscarModel)

  def solve(verbosity: Int, displaySolution: Boolean, fileName: String, timeout: Int): Unit = {

    val neighList = List(
      simpleNeighborhoods.insertNode(10),
      simpleNeighborhoods.moveOneNode(10),
      simpleNeighborhoods.twoOpt(10)
    )
  }

}
