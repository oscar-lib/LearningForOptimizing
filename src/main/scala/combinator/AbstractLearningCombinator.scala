package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{AcceptanceCriterion, SearchResult,NoMoveFound}
import oscar.cbls.core.search.Move
import oscar.cbls.core.search.MoveFound




abstract class AbstractLearningCombinator(name : String) extends Neighborhood(name) {

  def getNextNeighborhood : Neighborhood

  def learn(m : SearchResult,
    neighborhood : Neighborhood) : Unit

  def continue : Boolean

  override def getMove(obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion): SearchResult = {

    var continueSearch = true
    var currentResult : SearchResult = NoMoveFound

    while (continueSearch) {
      val neighborhood = getNextNeighborhood

      currentResult = neighborhood.getMove(obj,initialObj,acceptanceCriterion)

      learn(currentResult,neighborhood)


      currentResult match {
        case MoveFound(m) =>
          continueSearch = false
        case NoMoveFound =>
          continueSearch = continue
      }

    }

    currentResult

  }
}


 
