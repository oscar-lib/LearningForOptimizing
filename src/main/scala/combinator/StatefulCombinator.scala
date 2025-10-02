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
import bridge.SerializableModel
import oscar.cbls.core.search.MoveFound

object RLAlgorithm extends Enumeration {
  final val DQN = Value("dqn")
  final val PPO = Value("ppo")
}

class StatefulCombinator(
  neighborhoods: List[Neighborhood],
  model: SerializableModel,
  algo: RLAlgorithm.Value,
  debug: Boolean,
  ddqn: Boolean,
  lr: Double,
  clipping: Double,
  epsilon: Double,
  device: String,
  batchSize: Int,
  objective: Objective,
  acceptanceCriterion: AcceptanceCriterion,
  loadFrom: Option[String],
  training: Boolean,
  rewardModel: RewardModel,
  useTarget: Boolean,
  saveTo: Option[String] = None,
  seed: Int = 42
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme = AfterEveryMove, // Not used
      seed = seed,                     // Not used
      rewardModel = rewardModel,
      learningRate = 0.0 // Not used
    ) {

  private var lastMoveWasRandom = false
  private val nActions          = neighborhoods.length
  private val bridge =
    NamedPipeBridge(
      algo,
      debug,
      batchSize,
      epsilon,
      clipping,
      lr,
      ddqn,
      device,
      loadFrom,
      saveTo,
      training,
      useTarget
    )
  bridge.sendStaticProblemData(model, this.nActions)
  var justReset = false

  override def getMove(
    obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion
  ): SearchResult = {
    return super.getMove(obj, initialObj, this.acceptanceCriterion)
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    if (this.nTabu == this.nNeighbors) {
      return None
    } else if (this.model.hasObjectivePenalty()) {
      this.lastMoveWasRandom = true
      return this.getRandomNeighborhood
    }
    this.lastMoveWasRandom = false
    val action = this.bridge.askAction(this.model, this.authorizedNeighborhood)
    Some(this.neighborhoods(action))
  }

  override def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    this.justReset = false
    if (searchResult == NoMoveFound) {
      this.setTabu(neighborhood)
    }
    if (this.lastMoveWasRandom) {
      return
    }
    val stats  = NeighborhoodStats(searchResult, neighborhood)
    val reward = this.rewardModel(stats, neighborhood)
    val obj    = this.rewardModel.transformObjective(this.objective.value)
    this.bridge.sendReward(reward, obj)
  }

  override def reset(): Unit = {
    if (!this.justReset && !this.lastMoveWasRandom) {
      this.bridge.sendEpisodeEnded()
    }
    super.reset()
    this.justReset = true
  }

  def close() = {
    this.bridge.close()
  }

}
