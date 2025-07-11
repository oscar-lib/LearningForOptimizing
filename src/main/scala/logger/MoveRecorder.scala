package logger

import oscar.cbls.core.search.{Neighborhood, SearchResult}

import scala.collection.mutable.ArrayBuffer

class MoveRecorder {

  private val history: ArrayBuffer[Action] = new ArrayBuffer[Action]()

  def notifySearchResult(neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    val action = new MoveAction(neighborhood, searchResult)
    history.append(action)
  }

  def notifyReset(): Unit = {
    val action = new ResetAction()
    history.append(action)
  }

  override def toString: String = {
    history.mkString("[", ", ", "]")
  }

}

abstract sealed class Action

class ResetAction extends Action {
  override def toString: String = {
    s"(reset)"
  }
}

class MoveAction(
                  val neighborhood: Neighborhood,
                  val searchResult: SearchResult
                ) extends Action {

  override def toString: String = {
    s"(${neighborhood}, ${
      searchResult.getClass.getSimpleName
        .replace("$", "")
    })"
  }

}