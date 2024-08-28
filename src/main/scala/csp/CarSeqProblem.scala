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

package csp

/** This class represents an instance of the car sequencing problem (CSP).
  *
  * The CSP is concerned with finding a feasible production ordering of the given set of cars. Each
  * car belongs to a configuration, and each configuration is defined by a subset of the available
  * options (e.g. as air conditioning). For each option, two positive integers are provided: a
  * subsequence length and the maximum number of cars using that option that can be produced in any
  * subsequence of that length. An ordering is feasible if this subsequence constraint is satisfied
  * for all options.
  *
  * @param nCars
  *   total number of cars
  * @param nOptions
  *   number of different options each car can have
  * @param nConf
  *   total number of configurations, each defined by a subset of options
  * @param maxCarsWithOptInSeq
  *   maximum number of cars in a subsequence with the given option
  * @param optSeqLen
  *   subsequence length for the option constraint
  * @param configs
  *   list of configurations
  */
case class CarSeqProblem(
  nCars: Int,
  nOptions: Int,
  nConf: Int,
  maxCarsWithOptInSeq: Array[Int], // max num of cars with given option in subsequence
  optSeqLen: Array[Int],           // subsequence length
  configs: List[CarSeqConf]
) {

  /** Checks whether this CSP instance is valid or not. */
  def isValid: Boolean = {
    // need one of these values for each option
    if ((maxCarsWithOptInSeq.length != nOptions) || (optSeqLen.length != nOptions)) return false

    for (i <- optSeqLen.indices) {
      if (maxCarsWithOptInSeq(i) > optSeqLen(i)) return false
    }

    if (configs.length != nConf) return false

    // sum total of cars with over all configurations must be equal to given number of cars
    if (configs.map(_.nCarsWithConf).sum != nCars) return false

    // option-config indicator array must have one boolean per option
    for (c <- configs) {
      if (c.optInConf.length != nOptions) return false
    }
    true
  }
}

/** Class defining a configuration for a CSP instance.
  *
  * @param id
  *   identification number
  * @param nCarsWithConf
  *   total number of cars with this configuration
  * @param optInConf
  *   0-1 array defining which options belong to this configuration
  */
case class CarSeqConf(id: Int, nCarsWithConf: Int, optInConf: Array[Int])
