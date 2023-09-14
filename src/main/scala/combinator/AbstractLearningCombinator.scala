package combinator

import oscar.cbls.core.search._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.profiling.SelectionProfiler

import scala.annotation.tailrec

/**
  * An abstract combinator that can be used to implement learning.
  *
  * The search for a SearchResult works with a loop. The loop works the following
  *   - We get a Neighborhood Option with [[getNextNeighborhood]]
  *   - If this Neighborhood Option is defined (Some(n)), we use the given neighborhood to get a SearchResult
  *   - We continue until
  *     - Either the SearchResult for the Neighborhood is MoveFound (it means that a move has been found)
  *     - Or the neighborhood Option is not defined (None) (it means that we don't have neighborhood any more)
  * 
  * After each result that has been got from the neighborhood, the abstract method [[learn]] is called. This
  * method is used to update informations on the previous use of the neighborhood.
  * 
  * @param name The name of the Combinator
  */

abstract class AbstractLearningCombinator(name : String, neighborhoods: Neighborhood*) extends NeighborhoodCombinator(neighborhoods: _*) {

  private val _profiler: SelectionProfiler = new SelectionProfiler(this, neighborhoods.toList)
  override def profiler: SelectionProfiler = _profiler

  /** The method that provides a neighborhood.
    *
    * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
    */
  def getNextNeighborhood : Option[Neighborhood]

  /** The methods that "learns" from the results of the neighborhoods.
    *
    * @param m the last search result obtain
    * @param neighborhood the neighborhood from which the search result has been obtained
    */
  def learn(m : SearchResult,
    neighborhood : Neighborhood) : Unit

  override def getMove(obj: Objective,
    initialObj: Long,
    acceptanceCriterion: AcceptanceCriterion): SearchResult = {

    @tailrec
    def doSearch() : SearchResult = {
      getNextNeighborhood match {
        case None => NoMoveFound
        case Some(n) =>
          val candidateResult = n.getProfiledMove(obj,initialObj,acceptanceCriterion)
          learn(candidateResult,n)
          candidateResult match {
            case NoMoveFound => doSearch()
            case MoveFound(_) => candidateResult
          }
      }
    }

    doSearch()
  }
}
