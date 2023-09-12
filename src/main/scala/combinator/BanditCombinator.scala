package combinator

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

import collection.mutable.{ArrayDeque => Deque}
import scala.annotation.tailrec
import scala.util.Random


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
class BanditCombinator(l : List[Neighborhood],
  restartNeigh : Neighborhood,
  maxRestart : Int,
  obj : Objective,
  computeReward : Array[NeighborhoodStatistics] => Array[Double],
  ignoreFst : Boolean = true,
  rollingAverage: Boolean = false
) extends AbstractLearningCombinator("BanditCombinator", l: _*){

  private val nbNeigh : Int = l.length

  // private var bestSolution = obj.model.solution(true)
  // private var bestKnown = obj.value

  private val originalNeighProbability: Array[Double] = Array.fill(nbNeigh)(1D / nbNeigh)
  private val neighProbability : Array[Double] = Array.fill(nbNeigh)(1D/ nbNeigh)
  // private val shortTermNeighProbability : Array[Double] = Array.tabulate(nbNeigh)(i => longTermNeighProbability(i))


  private val rand = new Random(3000)
  private var authorizedNeighborhood : Array[Boolean] = Array.fill(nbNeigh)(true)
  private var nbAvailableNeigh = nbNeigh
  private var totalCurrentNeighWeight : Double = 1
  private var currentNbRestart = 0
  private var currentIndex = 0
  private var nbConsideredRestart = 0
  protected val neighStatistics : Array[NeighborhoodStatistics] = Array.fill(nbNeigh)(NeighborhoodStatistics())
  private var totalRewardRounds: Double = 100

  private def reinitStats() : Unit = {
    for (i <- 0 until nbNeigh)
      neighStatistics(i) = NeighborhoodStatistics()
  }

  private def reinitTabu() : Unit = {
    nbAvailableNeigh = nbNeigh
    totalCurrentNeighWeight = 1
    for (i <- 0 until nbNeigh) authorizedNeighborhood(i) = true
  }

  @tailrec
  private def getIndex(proba : Double,res : Int,accu : Double) : Int = {
    //println(s"$proba $accu $res ${authorizedNeighborhood(res)} ${neighProbability.mkString(";")}")
    val nextIndex = if (res >= nbNeigh - 1) 0 else res + 1
    if (authorizedNeighborhood(res)) {
      if (neighProbability(res) + accu > proba)
        res
      else {
        if (neighProbability(res) == 0)
          getIndex(proba,nextIndex,accu + 0.1)
        else
          getIndex(proba,nextIndex,accu + neighProbability(res))
      }
    } else {
      getIndex(proba,nextIndex,accu)
    }
  }

  private def getNextIndex : Int = {
    val nextFloat = rand.nextDouble() * totalCurrentNeighWeight
    //println("GetIndex")
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
        // obj.model.restoreSolution(bestSolution)
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

  def updateProbability(reward : Array[Double]) : Unit = {
    println(neighProbability.mkString(";"))
    var cumulativeRound : Double = 0
    var t : Double = 0
    for (i <- 0 until nbNeigh) {
      neighProbability(i) = neighProbability(i) * ((reward(i)+totalRewardRounds)/totalRewardRounds);
      cumulativeRound = cumulativeRound + (reward(i)).abs;
      t = t + neighProbability(i)
    }
    for (i <- 0 until nbNeigh) {
      neighProbability(i) = neighProbability(i) / t
    }
    totalRewardRounds = totalRewardRounds + cumulativeRound
    if (totalRewardRounds > 2000) {
      totalRewardRounds = totalRewardRounds - 1000;
      for (i <- 0 until nbNeigh) {
        neighProbability(i) = neighProbability(i) * 0.5 + 1/nbNeigh
      }
    }
    println(neighProbability.mkString(";"))
    nbConsideredRestart += 1
  }

  private val rewardHistory: Deque[Array[Double]] = Deque()
  private val cumulativeReward: Array[Double] = new Array[Double](nbNeigh)
  private val maxHistory = 5

  def updateProbabilityRollingAverage(reward : Array[Double]) : Unit = {
    rewardHistory.addOne(reward)
    for (i <- reward.indices) cumulativeReward(i) += reward(i)
    if (rewardHistory.length > maxHistory) {
      val old = rewardHistory.removeHead()
      for (i <- old.indices) cumulativeReward(i) -= old(i)
    }
    println("Probabilities before applying rewards : " + neighProbability.mkString(";"))
    for (i <- neighProbability.indices) {
      neighProbability(i) = (originalNeighProbability(i) + cumulativeReward(i)) / (maxHistory + 1)
    }
    println("Probabilities after applying rewards : " + neighProbability.mkString(";"))
    nbConsideredRestart += 1
  }

  // def rewardPerNeighborhood : Array[Int]

  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    if (neighborhood != restartNeigh) {
      val profilingData = NeighborhoodUtils.getProfiler(neighborhood)
      val stats = neighStatistics(currentIndex)
      m match {
        case NoMoveFound =>
          neighStatistics(currentIndex) =
            NeighborhoodStatistics(stats.nbCall + 1,
              0,  //stats.nbFound,
              1,  //stats.nbNotFound + 1,
              stats.totalTimeNano + profilingData._lastCallDurationNano,
              stats.totalTimeNotFoundNano + profilingData._lastCallDurationNano,
              stats.totalGain + profilingData._lastCallGain)
          addToTabu(currentIndex)
        case MoveFound(m) =>
          neighStatistics(currentIndex) =
            NeighborhoodStatistics(
              stats.nbCall + 1,
              1, //stats.nbFound + 1,
              0, //stats.nbNotFound,
              stats.totalTimeNano + profilingData._lastCallDurationNano,
              stats.totalTimeNotFoundNano,
              stats.totalGain + profilingData._lastCallGain
          )
          reinitTabu()
      }
      updateProbability(computeReward(neighStatistics).toArray)
    } else {
      // if (obj.value < bestKnown) {
      //   bestKnown = obj.value
      //   bestSolution = obj.model.solution(true)
      // }
      if (!ignoreFst || currentNbRestart > 1) {
        if (rollingAverage) {
          updateProbabilityRollingAverage(computeReward(neighStatistics))
        } else {
          updateProbability(computeReward(neighStatistics))
        }
      }
      reinitTabu()
    }
  }

}
