package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.search.SearchResult
import oscar.cbls.algo.magicArray.MagicBoolArray
import oscar.cbls.algo.heap.BinomialHeapWithMove
import oscar.cbls.core.search.NoMoveNeighborhood
import oscar.cbls.core.search.NoMoveFound
import oscar.cbls.core.search.MoveFound
import scala.annotation.tailrec

class BestSlopeFirstLearningWay(l : List[Neighborhood]) extends AbstractLearningCombinator("NewBSF") {

  case class NeighborhoodData(slope : Float,
    exhausted : Boolean)

  private val neighborhoodArray = l.toArray
  private val neighborhoodSlope : Array[Long] = neighborhoodArray.map(_ => Long.MaxValue)
  private val neighborhoodHeap = new BinomialHeapWithMove[Int](neighborhoodSlope,l.length)
  private var currentNeighborhoodIndex : Int = -1
  private var tabuNeighborhoodIndex : List[Int] = Nil

  @tailrec
  private def insertNeighborhoodList(l : List[Int]) : Unit= {
    l match {
      case Nil =>
      case h :: t =>
        neighborhoodHeap.insert(h)
        insertNeighborhoodList(t)
    }
  }

  insertNeighborhoodList(l.indices.toList)

  override def getNextNeighborhood: Option[Neighborhood] = {
    if (neighborhoodHeap.isEmpty)
      None
    else {
      currentNeighborhoodIndex = neighborhoodHeap.getFirst
      Some(neighborhoodArray(currentNeighborhoodIndex))
    }
  }

  override def learn(m: SearchResult,
    neighborhood: Neighborhood): Unit = {
    m match {
      case NoMoveFound =>
        tabuNeighborhoodIndex = neighborhoodHeap.removeFirst():: tabuNeighborhoodIndex
      case MoveFound(_) =>
        neighborhoodSlope(currentNeighborhoodIndex) = neighborhood.profiler.commonProfilingData.gain/Math.max(neighborhood.profiler.commonProfilingData.timeSpentMillis,1)
        neighborhoodHeap.notifyChange(currentNeighborhoodIndex)
        insertNeighborhoodList(tabuNeighborhoodIndex)
    }
  }

}
