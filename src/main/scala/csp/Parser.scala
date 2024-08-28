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

import java.io.File
import scala.io.Source

object Parser {
  def apply(file: File): CarSeqProblem = { // maybe add error handling
    val s     = Source.fromFile(file)
    val lines = s.getLines()

    val Array(nCars, nOptions, nConf) = lines.next().split("\\s").map(_.toInt)
    val maxCars                       = lines.next().split("\\s").map(_.toInt)
    val seqLen                        = lines.next().split("\\s").map(_.toInt)

    val confList: List[CarSeqConf] = {
      val b = List.newBuilder[CarSeqConf]
      while (lines.hasNext) {
        val arr = lines.next().split("\\s").map(_.toInt)
        require(arr.length == 2 + nOptions, "Invalid file")
        val Array(id, nCarsInConf) = arr.take(2)
        val optInConf              = arr.drop(2)
        b += CarSeqConf(id, nCarsInConf, optInConf)
      }
      b.result()
    }

    s.close()

    CarSeqProblem(
      nCars = nCars,
      nOptions = nOptions,
      nConf = nConf,
      maxCarsWithOptInSeq = maxCars,
      optSeqLen = seqLen,
      configs = confList
    )
  }
}
