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

import oscar.cbls._

import scala.util.Random

object Model {
  def apply(instance: CarSeqProblem): Model = {
    new Model(instance)
  }
}

class Model(instance: CarSeqProblem) {

  private val s = new Store()

  private val carArray: Array[Int] = {
    val b   = Array.newBuilder[Int]
    val ids = instance.configs.map(x => (x.id, x.nCarsWithConf))
    ids.foldLeft(b)({ case (b, (id, n)) =>
      for (_ <- 0 until n) b += id
      b
    })
    Random.shuffle(b.result()).toArray
  }

  val carSequence: Array[CBLSIntVar] = Array.tabulate(instance.nCars)(i => {
    CBLSIntVar(s, carArray(i), 0 until instance.nConf, s"car configuration at position $i")
  })

  val c = ConstraintSystem(s)

  for (opt <- 0 until instance.nOptions) {

    val configurationsWithOption: Array[Boolean] = {
      val arr = Array.fill[Boolean](instance.nConf)(false)
      for (c <- instance.configs) {
        if (c.optInConf(opt) == 1) arr(c.id) = true
      }
      arr
    }

    c.post(
      sequence(
        carSequence,
        instance.optSeqLen(opt),
        instance.maxCarsWithOptInSeq(opt),
        configurationsWithOption
      )
    )
  }

  val varViolation     = c.violations(carSequence)
  val violatedCars     = filter(varViolation)
  val mostViolatedCars = argMax(varViolation)

  c.close()

  val obj: Objective = c.violation

  s.close()
}
