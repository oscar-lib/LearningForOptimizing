package tsp

import java.io.File
import scala.xml._

object Parser {
  // The result of the benchmark is usually expressed in double.
  // We cannot deal with double within oscar therefore we multiply it by a factor and then divide it again.
  private val multiplierFactor = 1000

  /**
   * Parses a TSP instance written in xml
   * @param file file where the instance data is written
   * @return TSP instance corresponding to file
   */
  def apply(file: File): Problem = {
    // Load the XML file
    val root = XML.loadFile(file)

    // Extract all the <vertex> elements
    val vertices = (root \ "graph" \ "vertex")
    val nCities  = vertices.size

    // Initialize the distances matrix
    val distances = Array.ofDim[Long](nCities, nCities)

    // Fill the distances matrix
    for (i <- 0 until nCities) {
      val edges = (vertices(i) \ "edge")
      for (edge <- edges) {
        // Read the cost (which is in the "cost" attribute)
        val costAttr = (edge \ "@cost").text.trim
        // Convert scientific-notation string to a Double, then to Long (or round it)
        val cost = math.round(costAttr.toDouble * multiplierFactor)

        // The text content of <edge> is the index of the destination city
        val j = edge.text.trim.toInt

        distances(i)(j) = cost
      }
    }

    // Construct the Problem object
    Problem(nCities, distances, multiplierFactor)
  }
}
