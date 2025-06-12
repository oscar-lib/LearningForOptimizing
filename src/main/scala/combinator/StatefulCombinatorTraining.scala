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

class StatefulCombinatorTraining(
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
  seed: Int = 42
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme = AfterEveryMove, // Not used
      seed: Int,                       // Not used
      learningRate = 0.0,              // Not used
      rewardModel = new LogGain()
    ) {

  private val nActions = neighborhoods.length
  private val bridge = NamedPipeBridge(algo, debug, batchSize, epsilon, clipping, lr, ddqn, device)
  bridge.sendStaticProblemData(model, this.nActions)

  override def getMove(
    obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion = StrictImprovement
  ): SearchResult = {
    if (this.nTabu == this.nNeighbors) {
      return NoMoveFound
    }
    val action        = this.bridge.askAction(this.model, this.authorizedNeighborhood)
    val neighbourhood = this.neighborhoods(action);
    neighbourhood.getMove(obj, initialObj, AcceptAll) match {
      case MoveFound(result) =>
        val reward = this.rewardModel(initialObj, result.objAfter)
        this.bridge.sendReward(reward)
        return result
      case NoMoveFound =>
        return NoMoveFound
    }
  }

  private def getAvailableActions(): List[Int] = {
    (0 until nActions).filterNot(isTabu).toList
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    // If no available actino remains, return None
    // if (this.nTabu == this.nActions) {
    // return None
    // }
    val action = this.bridge.askAction(this.model, this.authorizedNeighborhood)
    Some(this.neighborhoods(action))
  }

  override def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    if (searchResult == NoMoveFound) {
      this.setTabu(neighborhood)
    }
    val stats  = NeighborhoodStats(searchResult, neighborhood)
    val reward = this.rewardModel(stats, neighborhood)
    this.bridge.sendReward(reward)
  }

  override def reset(): Unit = {
    this.bridge.sendEpisodeEnded()
    super.reset()
  }

  def close() = {
    this.bridge.close()
  }

}
