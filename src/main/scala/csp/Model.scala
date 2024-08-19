package csp

import scala.collection.immutable.SortedMap

object Model {
  def apply(instance: CarSeqProblem): Model = {
    new Model(instance)
  }
}

class Model(instance: CarSeqProblem) {

  val orderedCarsByType: SortedMap[Int,Int] = ???


}
