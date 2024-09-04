package util

import java.io.File
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

/** Helper case class for passing input values.
  *
  * @param file
  *   input instance file path
  * @param verbosity
  *   output verbosity level
  * @param bandit
  *   name of the bandit algorithm
  * @param display
  *   whether to display solving output or not
  * @param timeout
  *   weak timeout duration (in seconds)
  * @param learningRate
  *   bandit learning rate
  * @param slopeWeight
  *   learning weight of a move's slope
  * @param efficiencyWeight
  *   learning weight of a move's efficiency w.r.t. elapsed time
  * @param moveFoundWeight
  *   learning weight of a neighborhood finding a move
  * @param epsilon
  *   the initial value of the parameter for the epsilon-greedy bandit
  * @param confidence
  *   the weight of the confidence width for the UCB bandit
  */
case class SolverInput(
  file: File,
  verbosity: Int,
  bandit: String,
  display: Boolean,
  timeout: Int,
  learningRate: Double,
  slopeWeight: Double,
  efficiencyWeight: Double,
  moveFoundWeight: Double,
  epsilon: Double,
  confidence: Double
)
