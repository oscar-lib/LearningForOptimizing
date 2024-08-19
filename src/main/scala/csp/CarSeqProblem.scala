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

case class CarSeqProblem(
  nCars: Int,
  nOptions: Int,
  nConf: Int,
  maxCarsWithOptInSeq: Array[Int], // max num of cars with given option in subsequence
  optSeqLen: Array[Int],           // subsequence length
  configs: List[CarSeqConf]
) {
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

case class CarSeqConf(id: Int, nCarsWithConf: Int, optInConf: Array[Int])
