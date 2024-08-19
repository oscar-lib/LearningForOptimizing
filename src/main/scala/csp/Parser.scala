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
        val arr                    = lines.next().split("\\s").map(_.toInt)
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
