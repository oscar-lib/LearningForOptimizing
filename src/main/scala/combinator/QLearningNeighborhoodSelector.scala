package combinator

import oscar.cbls.core.search.Neighborhood
import bridge.PythonBridge
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.model.VRP

class QLearningNeighborhoodSelector(
  neighborhoods: List[Neighborhood],
  store: Store,
  problem: VRP,
  learningScheme: LearningScheme = AfterEveryMove,
  seed: Int = 42,
  learningRate: Double = 0.1
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme: LearningScheme,
      seed: Int,
      learningRate: Double
    ) {

  private val nActions = neighborhoods.length
  private val bridge   = new PythonBridge()

  def getCurrentSearchState() = {
    println(this.problem)
    val vars = this.store.decisionVariables().map(v => (v.name, v.valueString))
    println(vars)
    val state = vars.mkString(",")
    println(state)
    state

  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    val state = this.getCurrentSearchState()
    this.getBestNeighborhood
  }

  override def reward(runStat: NeighborhoodStats, neighborhood: Neighborhood): Double = {
    0.0
  }

}
