package tsp

case class Problem(
  nCities: Int,
  distances: Array[Array[Long]], // distance between each pair of cities
  // multiplier used to transform distances from the instance file (double) into distances suited for oscar (integers)
  multiplierFactor: Int
) {}
