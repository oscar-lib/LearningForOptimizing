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

/** Learning scheme considered
  */
abstract sealed class LearningScheme

/** Learning scheme performed after every move
  */
case object AfterEveryMove extends LearningScheme

/** Learning scheme performed after n moves
  * @param n
  *   number of consecutive moves that must be performed before learning
  */
case class AfterNMoves(n: Int) extends LearningScheme {
  // stores the current consecutive number of moves performed
  private var _nMoves: Int = 0

  /** Gives the current consecutive number of moves performed since the reset of the learning
    * scheme.
    */
  def nMoves: Int = _nMoves

  /** Tells that a new move was performed.
    */
  def incrementCounter(): Unit = _nMoves += 1

  /** Reset the counter. This should be called right after having learned.
    */
  def resetCounter(): Unit = _nMoves = 0

  /** Tells if the number of consecutive moves has been reached. The counter should be reset after
    * having learnt
    */
  def isCriterionMet: Boolean = _nMoves >= n
}

/** Learning scheme performed after every descent
  */
case object AfterEveryDescent extends LearningScheme
