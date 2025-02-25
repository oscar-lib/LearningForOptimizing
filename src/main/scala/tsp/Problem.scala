package tsp

case class Problem(
  nCities: Int,
  distances: Array[Array[Long]],
  multiplierFactor: Int
) {}
