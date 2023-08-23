package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.search.SearchResult
import oscar.cbls.algo.magicArray.MagicBoolArray
import oscar.cbls.algo.heap.BinomialHeapWithMove

class BestSlopeFirstLearningWay(l : List[Neighborhood]) extends AbstractLearningCombinator("NewBSF") {

  case class NeighborhoodData(slope : Float,
    exhausted : Boolean)

  private val candidateNeighborhood = new MagicBoolArray(l.length,true)
  private val neighborhoodArray = l.toArray
  private val neighborhoodSlope = neighborhoodArray.map(_ => Long.MaxValue)
  private val neighborhoodHeap = new BinomialHeapWithMove[Int](i => i.toLong,l.length)



  override def getNextNeighborhood: Neighborhood = {
    l(0)
  }

  override def learn(m: SearchResult,
    neighborhood: Neighborhood): Unit = {
  }


  override def continue : Boolean = true



}
