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
