package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound
import oscar.cbls.core.search.MoveFound
import scala.util.Random
import scala.annotation.tailrec
import oscar.cbls.core.computation.IntValue


case class NeighborhoodStatistics(
  nbCall : Int = 0,
  nbFound : Int = 0,
  nbNotFound : Int = 0,
  totalTimeNano : Long = 0,
  totalTimeNotFoundNano : Long = 0,
  totalGain : Long = 0)

/*
 * Score
 * aggrégation historique
 * Choix des voisinages
 */
abstract class BanditCombinator(l : List[Neighborhood],
  restartNeigh : Neighborhood,
  maxRestart : Int) extends AbstractLearningCombinator("BanditCombinator"){

  private val nbNeigh : Int = l.length

  private val neighProbability : Array[Double] = Array.fill(nbNeigh)(1D/ nbNeigh)
  // private val shortTermNeighProbability : Array[Double] = Array.tabulate(nbNeigh)(i => longTermNeighProbability(i))


  private val rand = new Random(3000)
  private var authorizedNeighborhood : Array[Boolean] = Array.fill(nbNeigh)(true)
  private var nbAvailableNeigh = nbNeigh
  private var totalCurrentNeighWeight : Double = 1
  private var currentNbRestart = 0
  private var currentIndex = 0
  protected val neighStatistics : Array[NeighborhoodStatistics] = Array.fill(nbNeigh)(NeighborhoodStatistics())

  private def reinitStats : Unit = {
    for (i <- 0 until nbNeigh)
      neighStatistics(i) = NeighborhoodStatistics()
  }

  private def reinitTabu : Unit = {
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

  def updateProbability(reward : Array[Int]) : Unit = {

  }

  def rewardPerNeighborhood : Array[Int]

  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    if (neighborhood != restartNeigh) {
      val profilingData = NeighborhoodUtils.getProfilingData(neighborhood)
      val stats = neighStatistics(currentIndex)
      m match {
        case NoMoveFound =>
          neighStatistics(currentIndex) =
            NeighborhoodStatistics(stats.nbCall + 1,
              stats.nbFound,
              stats.nbNotFound + 1,
              stats.totalTimeNano + profilingData._lastCallDurationNano,
              stats.totalTimeNotFoundNano + profilingData._lastCallDurationNano,
              stats.totalGain + profilingData._lastCallGain)
          addToTabu(currentIndex)
        case MoveFound(m) =>
          neighStatistics(currentIndex) =
            NeighborhoodStatistics(
              stats.nbCall + 1,
              stats.nbFound + 1,
              stats.nbNotFound,
              stats.totalTimeNano + profilingData._lastCallDurationNano,
              stats.totalTimeNotFoundNano + profilingData._lastCallDurationNano,
              stats.totalGain + profilingData._lastCallGain
          )
          reinitTabu
      }
    } else {
      updateProbability(rewardPerNeighborhood)
      reinitTabu
    }
  }




}

