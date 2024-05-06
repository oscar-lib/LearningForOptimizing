// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package combinator

import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

import scala.util.Random

/**
 * A combinator returning a random valid neighborhood to try
 * Invalid neighborhood are placed in a tabu list and not tried immediately afterwards
 * @param l the list of neighborhoods to be tried randomly
 */
class RandomCombinator(l : List[Neighborhood]) extends AbstractLearningCombinator("Random"){

  private val random = new Random()
  private val authorizedNeighborhood: Array[Boolean] = Array.fill(l.length)(true)
  private var nValid = l.length
  private var lastIndexTried = 0

  private def authorizeAll(): Unit = {
    for (i <- l.indices) authorizedNeighborhood(i) = true
    nValid = l.length
  }

  override def reset(): Unit = {
    authorizeAll()
    super.reset()
  }

  /** The method that provides a neighborhood.
   *
   * @return Some(n) if a neighborhood is available or None if the neighborhoods are exhausted
   */
  override def getNextNeighborhood: Option[Neighborhood] = {
    if (nValid == 0)
      None
    else {
      // draw a random number
      lastIndexTried = random.nextInt(nValid)
      // choose a random neighborhood that is marked as authorized
      lastIndexTried = authorizedNeighborhood.zipWithIndex.filter(_._1)(lastIndexTried)._2
      Some(l(lastIndexTried))
    }
  }

  /** The methods that "learns" from the results of the neighborhoods.
   *
   * @param m            the last search result obtain
   * @param neighborhood the neighborhood from which the search result has been obtained
   */
  override def learn(m: SearchResult, neighborhood: Neighborhood): Unit = {
    m match {
      case NoMoveFound =>
        authorizedNeighborhood(lastIndexTried) = false
        nValid -= 1
      case MoveFound(_) =>
        authorizeAll()
    }
  }
}
