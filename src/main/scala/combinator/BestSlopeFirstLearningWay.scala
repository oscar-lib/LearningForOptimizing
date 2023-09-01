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

 // The neighborhood in an Array form
  private val neighborhoodArray = l.toArray
  // The array to store the slope of the neighborhood
  private val neighborhoodSlope : Array[Long] = neighborhoodArray.map(_ => Long.MaxValue) 
  // A heap to get the neighborhood with the highest slope
  private val neighborhoodHeap = new BinomialHeapWithMove[Int](neighborhoodSlope,l.length + 1)
  // The current Neighborhood that has been used
  private var currentNeighborhoodIndex : Int = -1
  // The list of neighborhood that will not be used (because they did not find any moves)
  private var tabuNeighborhoodIndex : List[Int] = Nil
  // The list of index
  private val indicesList = l.indices.toList


  // A method to insert a list of neighborhood in the heap
  @tailrec
  private def insertNeighborhoodList(l : List[Int]) : Unit= {
    l match {
      case Nil =>
      case h :: t =>
        neighborhoodHeap.insert(h)
        insertNeighborhoodList(t)
    }
  }

  insertNeighborhoodList(indicesList)


  override def getNextNeighborhood: Option[Neighborhood] = {
    // If the heap is empty: we do not have any neighborhood
    if (neighborhoodHeap.isEmpty) {
      // Reseting the state of the neighborhood
      insertNeighborhoodList(indicesList)
      tabuNeighborhoodIndex = Nil
      None
    }
    else {
      // Getting the first neighborhood of the heap
      currentNeighborhoodIndex = neighborhoodHeap.getFirst
      Some(neighborhoodArray(currentNeighborhoodIndex))
    }
  }

  override def learn(m: SearchResult,
    neighborhood: Neighborhood): Unit = {
    m match {
      case NoMoveFound =>
        // If no move have been found, removing the neighborhood from the heap
        // and putting it in the tabu neighborhood list
        tabuNeighborhoodIndex = neighborhoodHeap.removeFirst():: tabuNeighborhoodIndex
      case MoveFound(_) =>
        // Updating the slope of the neighborhood
        neighborhoodSlope(currentNeighborhoodIndex) = - (neighborhood.profiler.commonProfilingData.gain * 1000)/Math.max(neighborhood.profiler.commonProfilingData.timeSpentMillis,1)
        // Notifying the heap so that it updates the positions
        neighborhoodHeap.notifyChange(currentNeighborhoodIndex)
        // Resetting the tabu list (maybe the last move deblocked some of them)
        insertNeighborhoodList(tabuNeighborhoodIndex)
        tabuNeighborhoodIndex = Nil
    }
  }

}
