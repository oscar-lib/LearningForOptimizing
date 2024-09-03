package combinator

import oscar.cbls.core.search.Neighborhood
import bridge.SocketBridge
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.model.VRP
import pdptw.LiLimProblem
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound
import bridge.NamedPipeBridge

class QLearningNeighborhoodSelector(
  neighborhoods: List[Neighborhood],
  problem: LiLimProblem,
  vrp: VRP,
  seed: Int = 42
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme = AfterEveryMove, // Not used
      seed: Int,                       // Not used
      learningRate = 0.0,              // Not used
      rewardModel = new NormalizedWindowedMaxGain(100)
    ) {

  private val nActions  = neighborhoods.length
  private val nVehicles = problem.vehicles.length
  // private val bridge    = SocketBridge(5555)
  val bridge = NamedPipeBridge()
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

  private def getAvailableActions(): List[Int] = {
    (0 until nActions).filterNot(isTabu).toList
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    val state  = this.getCurrentSearchState()
    val action = this.bridge.askAction(state)
    Some(this.neighborhoods(action))
  }

  override def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    val stats  = NeighborhoodStats(searchResult, neighborhood)
    val reward = this.rewardModel(stats, neighborhood)
    this.bridge.sendReward(reward)
  }

  override def reset(): Unit = {
    super.reset()
    this.bridge.sendEpisodeEnded()
  }
}
