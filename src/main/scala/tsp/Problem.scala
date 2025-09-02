package tsp

case class Problem(
  cityCoords: Array[Array[Double]], // Array of (x, y) coordinates for each city
  distances: Array[Array[Long]],    // distance between each pair of cities
  // multiplier used to transform distances from the instance file (double) into distances suited for oscar (integers)
  multiplierFactor: Int
) {
  def nCities(): Int = {
    this.distances.length
  }
}
