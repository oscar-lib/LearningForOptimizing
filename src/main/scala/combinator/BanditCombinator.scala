package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound
import oscar.cbls.core.search.MoveFound
import scala.util.Random
import scala.annotation.tailrec


case class NeighborhoodStatistics(
  nbCall : Int,
  nbFound : Int,
  nbNotFound : Int,
  totalTime : Long,
  totalGain : Long)

/*
 * Score
 * aggrégation historique
 * Choix des voisinages
 */
class BanditCombinator(l : List[Neighborhood],
  restartNeigh : Neighborhood,
  maxRestart : Int) extends AbstractLearningCombinator("BanditCombinator"){

  private val nbNeigh : Int = l.length

  private val neighProbability : Array[Double] = Array.fill(nbNeigh)(1D/ nbNeigh)
  // private val shortTermNeighProbability : Array[Double] = Array.tabulate(nbNeigh)(i => longTermNeighProbability(i))


  private val rand = new Random(2000)
  private var authorizedNeighborhood : Array[Boolean] = Array.fill(nbNeigh)(true)
  private var nbAvailableNeigh = nbNeigh
  private var totalCurrentNeighWeight : Double = 1
  private var currentNbRestart = 0
  private var currentIndex = 0
  protected val neighStatistics : Array[NeighborhoodStatistics] = Array.fill(nbNeigh)(NeighborhoodStatistics(0,0,0,0,0))

  private def reinit : Unit = {
    nbAvailableNeigh = nbNeigh
    totalCurrentNeighWeight = 1
    for (i <- 0 until nbNeigh) authorizedNeighborhood(i) = true
  }

  @tailrec
  private def getIndex(proba : Double,res : Int,accu : Double) : Int = {
    val nextIndex = if (res == nbNeigh) 0 else res + 1
    if (authorizedNeighborhood(res)) {
      if (neighProbability(res) + accu > proba)
        res
      else
        getIndex(proba,nextIndex,accu + neighProbability(res))
    } else {
      getIndex(proba,nextIndex,accu)
    }
  }

  private def getNextIndex : Int = {
    val nextFloat = rand.nextDouble() * totalCurrentNeighWeight
    getIndex(nextFloat,0,0)
  }

  private def addToTabu(n : Int) : Unit = {
    authorizedNeighborhood(n) = false
    totalCurrentNeighWeight -= neighProbability(n)
    nbAvailableNeigh -= 1
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    if (nbAvailableNeigh == 0) {
      if (currentNbRestart == maxRestart) {
        currentNbRestart = 0
        None
      }
      else {
        currentNbRestart += 1
        Some(restartNeigh)
      }
    }
    else {
      currentIndex = getNextIndex
      Some(l(currentIndex))
    }
  }



  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    if (neighborhood != restartNeigh) {
      m match {
        case NoMoveFound =>
          addToTabu(currentIndex)
        case MoveFound(m) =>
          reinit
      }
    } else
        reinit
  }




}

