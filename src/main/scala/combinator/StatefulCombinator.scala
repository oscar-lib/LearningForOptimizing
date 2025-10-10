package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.computation.Store
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.AcceptanceCriterion
import oscar.cbls.core.search.AcceptAll
import oscar.cbls.core.search.StrictImprovement
import bridge.SerializableModel
import oscar.cbls.core.search.MoveFound
import bridge.Bridge
import bridge.NamedPipeBridge
import bridge.UnixPipeBridge
import util.SolverInput

object RLAlgorithm extends Enumeration {
  final val DQN = Value("dqn")
  final val PPO = Value("ppo")
}

class StatefulCombinator(
  neighborhoods: List[Neighborhood],
  model: SerializableModel,
  args: SolverInput,
  objective: Objective,
  rewardModel: RewardModel
) extends BanditSelector(
      neighborhoods: List[Neighborhood],
      learningScheme = AfterEveryMove, // Not used
      seed = args.seed,                // Not used
      rewardModel = rewardModel,
      learningRate = 0.0 // Not used
    ) {

  private val nActions = neighborhoods.length
  private val bridge   = new UnixPipeBridge(args)
  bridge.sendStaticProblemData(model, this.nActions)
  private var prevObjective = this.objective.value
  private var justReset     = false

  override def getMove(
    obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion
  ): SearchResult = {
    return super.getMove(obj, initialObj, this.args.acceptanceCriterion)
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    if (this.nTabu == this.nNeighbors) {
      return None
    }
    val action = this.bridge.askAction(this.model, this.authorizedNeighborhood)
    Some(this.neighborhoods(action))
  }

  override def notifyMove(searchResult: SearchResult, neighborhood: Neighborhood): Unit = {
    if (searchResult == NoMoveFound) {
      this.setTabu(neighborhood)
    }
    val newObj         = this.objective.value
    val reward         = this.rewardModel(this.prevObjective, newObj)
    val transformedObj = this.rewardModel.transformObjective(newObj)
    this.bridge.sendReward(reward, transformedObj)
    this.prevObjective = newObj
    this.justReset = false
  }

  override def reset(): Unit = {
    if (!this.justReset) {
      this.bridge.sendEpisodeEnded()
    }
    super.reset()
    this.justReset = true
  }

  def close() = {
    this.bridge.close()
  }

}
