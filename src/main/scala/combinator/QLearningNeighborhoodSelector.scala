package combinator

import oscar.cbls.core.search.Neighborhood
import bridge.PythonBridge
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.model.VRP
import pdptw.LiLimProblem

class QLearningNeighborhoodSelector(
  neighborhoods: List[Neighborhood],
  store: Store,
  problem: LiLimProblem,
  vrp: VRP,
  learningScheme: LearningScheme = AfterEveryMove,
  seed: Int = 42,
  learningRate: Double = 0.1
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme: LearningScheme,
      seed: Int,
      learningRate: Double
    ) {

  private val nActions  = neighborhoods.length
  private val nVehicles = problem.vehicles.length
  private val bridge    = new PythonBridge()
  bridge.sendStaticProblemData(this.problem, this.nActions)

  private def getCurrentSearchState(): List[List[Int]] = {
    var routes: List[List[Int]] = List.empty
    for (vehicle <- 0 until this.vrp.v) {
      val routeOfV = this.vrp.getRouteOfVehicle(vehicle)
      if (routeOfV.length > 1) {
        routes = routes :+ routeOfV
      }
    }
    return routes
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    val state   = this.getCurrentSearchState()
    val stime   = System.nanoTime()
    val qvalues = this.bridge.askInference(state)
    val etime   = System.nanoTime()
    println(s"Time taken for inference: ${(etime - stime) / 1e6} milliseconds")

    val stime2 = System.nanoTime()
    val n      = this.getBestNeighborhood
    val etime2 = System.nanoTime()
    println(s"Time taken for bestNeighbor: ${(etime2 - stime2) / 1e6} milliseconds")
    n
  }

  override def reward(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    1.0
  }

}
