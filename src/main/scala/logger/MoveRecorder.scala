package logger

import oscar.cbls.Objective
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

import scala.collection.mutable.ArrayBuffer

class MoveRecorder(objective: Objective) {

  private val history: ArrayBuffer[Action] = new ArrayBuffer[Action]()

  def notifySearchResult(neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    val action = new MoveAction(neighborhood, searchResult, objective.value)
    history.append(action)
  }

  def notifyReset(): Unit = {
    val action = new ResetAction(objective.value)
    history.append(action)
  }

  override def toString: String = {
    history.mkString("[", ", ", "]")
  }

}

abstract sealed class Action

class ResetAction(value: Long) extends Action {
  override def toString: String = {
    s"(reset, MoveFound, $value)"
  }
}

class MoveAction(val neighborhood: Neighborhood, val searchResult: SearchResult, value: Long) extends Action {

  override def toString: String = {
    searchResult match {
      case NoMoveFound =>
        s"(${neighborhood}, ${searchResult.getClass.getSimpleName
            .replace("$", "") + ", " + value})"
      case MoveFound(m) =>
        s"(${neighborhood}, ${searchResult.getClass.getSimpleName
            .replace("$", "") + ", " + m.objAfter})"
    }

  }

}
