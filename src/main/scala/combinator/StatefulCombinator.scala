package combinator

import oscar.cbls.core.search.Neighborhood
import bridge.SocketBridge
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound
import bridge.NamedPipeBridge
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.AcceptanceCriterion
import oscar.cbls.core.search.AcceptAll
import oscar.cbls.core.search.StrictImprovement

object RLAlgorithm extends Enumeration {

  /** [bidirectional] Acknowledge message.
    */
  final val DQN = Value("dqn")
  final val PPO = Value("ppo")
}

class StatefulCombinator(
  neighborhoods: List[Neighborhood],
  model: Either[pdptw.Model, csp.Model],
  algo: RLAlgorithm.Value,
  debug: Boolean,
  ddqn: Boolean,
  lr: Double,
  clipping: Double,
  epsilon: Double,
  device: String,
  batchSize: Int,
  seed: Int = 42
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme = AfterEveryMove, // Not used
      seed: Int,                       // Not used
      learningRate = 0.0,              // Not used
      rewardModel = new LogGain()
    ) {

  private val nActions = neighborhoods.length
  // private val bridge    = SocketBridge(5555)
  val bridge =
    NamedPipeBridge(this.algo, this.debug, batchSize, epsilon, clipping, lr, ddqn, device)
  model match {
    case Left(value) => {
      bridge.sendStaticProblemData(value.liLimProblem, this.nActions)
    }
    case Right(value) => {
      bridge.sendStaticProblemData(value.instance, this.nActions)
    }
  }

  private def getCurrentSearchState(): List[List[Int]] = {
    this.model match {
      case Left(value)  => value.getState()
      case Right(value) => List()
    }
  }

  override def getMove(
    obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion = StrictImprovement
  ): SearchResult = {
    super.getMove(obj, initialObj, AcceptAll)
  }

  private def getAvailableActions(): List[Int] = {
    (0 until nActions).filterNot(isTabu).toList
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    val state  = this.getCurrentSearchState()
    val action = this.bridge.askAction(state, this.authorizedNeighborhood)
    Some(this.neighborhoods(action))
  }

  override def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    if (searchResult == NoMoveFound) {
      setTabu(neighborhood)
    }
    val stats  = NeighborhoodStats(searchResult, neighborhood)
    val reward = this.rewardModel(stats, neighborhood)
    if (reward < 0) {
      println("Negative reward: " + reward);
    }
    this.bridge.sendReward(reward)
  }

  override def reset(): Unit = {
    super.reset()
    this.bridge.sendEpisodeEnded()
  }

  def close() = {
    this.bridge.close()
  }

}
