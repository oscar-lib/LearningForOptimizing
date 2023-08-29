package combinator

import oscar.cbls.core.search.{AcceptanceCriterion, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}
import oscar.cbls.core.objective.Objective
import scala.annotation.tailrec




abstract class AbstractLearningCombinator(name : String) extends Neighborhood(name) {

  def getNextNeighborhood : Option[Neighborhood]

  def learn(m : SearchResult,
    neighborhood : Neighborhood) : Unit

  // def continue : Boolean

  override def getMove(obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion): SearchResult = {

    @tailrec
    def doSearch : SearchResult = {
      getNextNeighborhood match {
        case None => NoMoveFound
        case Some(n) =>
          val candidateResult = n.getMove(obj,initialObj,acceptanceCriterion)
          learn(candidateResult,n)
          candidateResult match {
            case NoMoveFound => doSearch
            case MoveFound(_) => candidateResult
          }
      }
    }

    doSearch

    // var continueSearch = true
    // var currentResult : SearchResult = NoMoveFound

    // while (continueSearch) {
    //   val neighborhood = getNextNeighborhood

    //   currentResult = neighborhood.getMove(obj,initialObj,acceptanceCriterion)

    //   learn(currentResult,neighborhood)


    //   currentResult match {
    //     case MoveFound(m) =>
    //       continueSearch = false
    //     case NoMoveFound =>
    //       continueSearch = continue
    //   }

    // }

    // currentResult

  }
}


 
