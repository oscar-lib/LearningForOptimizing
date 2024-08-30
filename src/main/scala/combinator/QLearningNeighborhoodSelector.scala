package combinator

import oscar.cbls.core.search.Neighborhood
import bridge.PythonBridge
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.model.VRP
import pdptw.LiLimProblem
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound

class QLearningNeighborhoodSelector(
  neighborhoods: List[Neighborhood],
  store: Store,
  problem: LiLimProblem,
  vrp: VRP,
  learningScheme: LearningScheme = AfterEveryMove,
  epsilon: Double = 0.1,
  seed: Int = 42,
  learningRate: Double = 0.1
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme: LearningScheme,
      seed: Int,
      learningRate: Double,
      rewardModel = new NormalizedWindowedMaxGain(100)
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

  /** Selects the next neighborhood to explore using an epsilon-greedy strategy.
    *
    * @param qvalues
    * @return
    *   the index of the neighborhood to explore
    */
  private def egreedy(qvalues: List[Double]): Option[Int] = {
    val availableActions = this.getAvailableActions()
    if (availableActions.isEmpty) {
      return None
    }
    val rand = scala.util.Random.nextDouble()
    if (rand < this.epsilon) {
      return Some((rand * availableActions.length).toInt)
    }
    var maxIndex = availableActions(0)
    var max      = qvalues(maxIndex)
    for (action <- availableActions.drop(1)) {
      if (qvalues(action) > max) {
        max = qvalues(action)
        maxIndex = action
      }
    }
    return Some(maxIndex)
  }

  private def getAvailableActions(): List[Int] = {
    var availableActions: List[Int] = List.empty
    for (i <- 0 until this.nActions) {
      if (!this.isTabu(i)) {
        availableActions = availableActions :+ i
      }
    }
    return availableActions
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    val state   = this.getCurrentSearchState()
    val qvalues = this.bridge.askInference(state)
    this.egreedy(qvalues) match {
      case None         => None
      case Some(action) => Some(this.neighborhoods(action))
    }
  }

  override def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    if (searchResult == NoMoveFound) {
      setTabu(neighborhood)
    }
    val stats  = NeighborhoodStats(searchResult, neighborhood)
    val reward = this.rewardModel(stats, neighborhood)
    this.bridge.sendReward(reward)
  }

  override def reward(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    val r = this.rewardModel(runStat, neighborhood)
    return r
  }

}
