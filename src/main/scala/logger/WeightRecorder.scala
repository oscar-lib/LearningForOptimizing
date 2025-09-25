package logger

import combinator.BanditSelector

import scala.collection.mutable.ArrayBuffer

class WeightRecorder(var banditSelector: BanditSelector) {

  def setBanditSelector(banditSelector: BanditSelector): Unit = {
    this.banditSelector = banditSelector
  }

  private val history: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]()

  def registerWeights() : Unit = {
    history.append(banditSelector.weightsCopy())
  }

  override def toString: String = {
    val names = banditSelector.neighborhoodNames().mkString("[", ", ", "]")
    val weights = history
      .map(w => w.mkString("(", ", ", ")"))
      .mkString("[", ", ", "]")
    s"neighborhoods=$names\nweights=$weights"
  }

}
