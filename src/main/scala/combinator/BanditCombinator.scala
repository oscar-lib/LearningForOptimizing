package combinator

import oscar.cbls.core.search.Neighborhood
import oscar.cbls.core.search.SearchResult
import oscar.cbls.core.search.NoMoveFound
import oscar.cbls.core.search.MoveFound
import scala.util.Random
import scala.annotation.tailrec


/*
 * Score
 * aggrégation historique
 * Choix des voisinages
 */
abstract class BanditCombinator(l : List[Neighborhood],
  restartNeigh : Neighborhood,
  maxRestart : Int) extends AbstractLearningCombinator("BanditAfterRestart"){

  private val neighborhoodArray : Array[Neighborhood] = l.toArray
  private val nbNeigh : Int = l.length
  private var availableNeighIndex : List[Int] = l.indices.toList
  private val rand = new Random(1000)
  protected val neighProbability : List[Double] = List.fill(nbNeigh)(1 / nbNeigh)

  private var nbRestarts = 0
  private var currentIndex = 0
  protected var neighborhoodsAndResults : List[(SearchResult,Neighborhood)] = Nil

  @tailrec
  private def getIndex(proba : Double,res : Int,accu : Double) : Int = {
    if (neighProbability(res) + accu > proba)
      res
    else
      getIndex(proba,res + 1, accu + neighProbability(res))
  }

  def getNextIndex : Int = {
    val scale = 1000000
    val nextFloat = rand.nextInt(scale).toDouble / scale
    getIndex(nextFloat,0,0)
  }

  def updateProbabilityAfterRestart : Unit = {
  }

  def updateProbailityAfterMove : Unit = {
  }

  override def getNextNeighborhood: Option[Neighborhood] = {
    availableNeighIndex match {
      case Nil =>
        if (nbRestarts == maxRestart)
          None
        else {
          nbRestarts += 1
          Some(restartNeigh)
        }
      case _ =>
        currentIndex = getNextIndex
        Some(l(currentIndex))
    }
  }


  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    if (neighborhood == restartNeigh) {
      neighborhoodsAndResults = Nil
    }
    else {
      neighborhoodsAndResults = (m,neighborhood) :: neighborhoodsAndResults
      m match {
        case NoMoveFound => availableNeighIndex = availableNeighIndex.filterNot(_ == currentIndex)
        case MoveFound(m) => availableNeighIndex = l.indices.toList
      }
    }
  }




}

