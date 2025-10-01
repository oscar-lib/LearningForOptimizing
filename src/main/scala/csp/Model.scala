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
import bridge.SerializableModel

import csp.CarSeqConf
import csp.CarSeqProblem
import oscar.cbls.business.routing.model.VRP
import pdptw.LiLimCouple
import pdptw.LiLimDepot
import pdptw.LiLimNode
import pdptw.LiLimProblem
import pdptw.LiLimVehicle
import upickle.default._

import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileReader
import java.io.InputStream
import java.io.OutputStream
import java.net.Socket
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Paths
import java.nio.file.Path
import bridge.MessageType

/** This object takes care of converting an instance of a car sequencing problem into a model in the
  * OscaR framework. Notably, the objective function under consideration is the number of violated
  * option utilization constraints.
  */
object Model {
  def apply(instance: CarSeqProblem): Model = {
    new Model(instance)
  }
}

import upickle.default.{macroRW, ReadWriter, Writer}

class Model(val instance: CarSeqProblem) extends SerializableModel {
  implicit val csconfRw: ReadWriter[CarSeqConf] = macroRW
  implicit val csRw: ReadWriter[CarSeqProblem]  = macroRW
  implicit val modelWriter: Writer[Model]       = macroRW

  private val store = new Store()

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
    CBLSIntVar(store, carArray(i), 0 until instance.nConf, s"car configuration at position $i")
  })

  val constraintSystem = ConstraintSystem(store)

  for (opt <- 0 until instance.nOptions) {

    val configurationsWithOption: Array[Boolean] = {
      val arr = Array.fill[Boolean](instance.nConf)(false)
      for (c <- instance.configs) {
        if (c.optInConf(opt)) arr(c.id) = true
      }
      arr
    }

    constraintSystem.post(
      sequence(
        carSequence,
        instance.optSeqLen(opt),
        instance.maxCarsWithOptInSeq(opt),
        configurationsWithOption
      )
    )
  }

  private val varViolation       = constraintSystem.violations(carSequence)
  val violatedCars: SetValue     = filter(varViolation)
  val mostViolatedCars: SetValue = argMax(varViolation)

  constraintSystem.close()

  val obj: Objective = constraintSystem.violation

  store.close()

  override def getJSONStaticProblemData(): String = {
    write(this)
  }

  override def getJSONState(): String = {
    val s = carSequence.map(_.value.intValue()).toList
    write(s)
  }

  override def getProblemCode(): MessageType.Value = MessageType.STATIC_DATA_CSP

  override def hasObjectivePenalty(): Boolean = false

  override def getNormalizationFactor(): Float = 1.0f
}
