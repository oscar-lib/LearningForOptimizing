package tsp

import java.io.File
import scala.xml._
import scala.io.Source
import scala.math._

object Parser {
  // The result of the benchmark is usually expressed in double.
  // We cannot deal with double within oscar therefore we multiply it by a factor and then divide it again.
  private val multiplierFactor = 1000

  /**
   * Parses a TSP instance file. This method chooses which specific parser to invoke
   * based on the file extension.
   */
  def apply(file: File): Problem = {
    if (file.getName.endsWith("xml")) {
      parseXMLFile(file)
    } else if (file.getName.endsWith("txt")) {
      parseTxtFile(file)
    } else if (file.getName.endsWith("tsp")) {
      parseTSPLibFile(file)
    } else {
      throw new IllegalArgumentException(
        s"Unrecognized extension for file ${file.getName} when parsing a TSP instance"
      )
    }
  }

  def parseXMLFile(file: File): Problem = {
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

  def parseTxtFile(file: File): Problem = {
    val source = Source.fromFile(file)
    try {
      val lines = source.getLines()

      // The first line is the number of cities
      val nCities = lines.next().toInt
      // The second line is the multiplier
      val multiplier = lines.next().toInt

      // Initialize the 2D array for distances
      val distances = Array.ofDim[Long](nCities, nCities)

      // For each of the next nCities lines, parse the row of integers
      for (i <- 0 until nCities) {
        val row = lines.next().split("\\s+").map(_.toLong)
        for (j <- 0 until nCities) {
          distances(i)(j) = row(j)
        }
      }

      // Build and return the Problem
      Problem(nCities, distances, multiplier)

    } finally {
      source.close()
    }
  }

  /**
   * Parses a TSP instance in TSPLib format:
   *  - Reads header (NAME, DIMENSION, EDGE_WEIGHT_TYPE, etc.)
   *  - Reads NODE_COORD_SECTION with nCities lines
   *  - Computes distances based on the specified EDGE_WEIGHT_TYPE
   *  - Scales each distance by multiplierFactor for integer usage
   */
  def parseTSPLibFile(file: File): Problem = {
    val lines = Source.fromFile(file).getLines().map(_.trim).toList
    if (lines.isEmpty)
      throw new IllegalArgumentException(s"File '${file.getName}' is empty or unreadable.")

    // --- 1) Extract essential header info ---
    val dimensionLine     = lines.find(_.toUpperCase.startsWith("DIMENSION")).getOrElse("")
    val edgeWeightTypeLine= lines.find(_.toUpperCase.startsWith("EDGE_WEIGHT_TYPE")).getOrElse("")
    // e.g. DIMENSION: 16 -> 16
    val nCities = dimensionLine.split(":", 2).lift(1).map(_.trim.toInt)
      .getOrElse(throw new IllegalArgumentException("DIMENSION not found or invalid."))

    // e.g. EDGE_WEIGHT_TYPE: GEO -> GEO
    val edgeWeightType = edgeWeightTypeLine.split(":", 2).lift(1).map(_.trim.toUpperCase)
      .getOrElse("EUC_2D") // default to EUC_2D if missing

    // --- 2) Locate NODE_COORD_SECTION and read the node coordinates ---
    val nodeCoordIndex = lines.indexWhere(_.toUpperCase == "NODE_COORD_SECTION")
    if (nodeCoordIndex < 0) {
      throw new IllegalArgumentException("NODE_COORD_SECTION not found in TSPLib file.")
    }

    val coordsLines = lines.slice(nodeCoordIndex + 1, nodeCoordIndex + 1 + nCities)
    if (coordsLines.size < nCities) {
      throw new IllegalArgumentException(
        s"Expected $nCities coordinate lines but found ${coordsLines.size}."
      )
    }

    // Parse each coordinate line: "id  x  y"
    // We'll ignore the city ID's position for indexing and just read in order.
    val coords: Array[(Double, Double)] = coordsLines.map { line =>
      val parts = line.split("\\s+").filter(_.nonEmpty)
      if (parts.length < 3)
        throw new IllegalArgumentException(s"Invalid node coordinate line: '$line'")

      // parts(0) => city ID, parts(1) => xCoord, parts(2) => yCoord
      val x = parts(1).toDouble
      val y = parts(2).toDouble
      (x, y)
    }.toArray

    // --- 3) Build the distance matrix ---
    val distances = Array.ofDim[Long](nCities, nCities)
    for (i <- 0 until nCities; j <- 0 until nCities) {
      if (i == j) {
        distances(i)(j) = 0
      } else {
        val (x1, y1) = coords(i)
        val (x2, y2) = coords(j)

        // Compute the raw (double) distance based on the edgeWeightType:
        val distDouble: Double = edgeWeightType match {
          case "EUC_2D" => euclidean2D(x1, y1, x2, y2)
          case "GEO"    => geoDistance(x1, y1, x2, y2)
          // Add more cases as needed, e.g., CEIL_2D, MAN_2D, etc.
          case other =>
            throw new UnsupportedOperationException(
              s"EDGE_WEIGHT_TYPE '$other' is not supported by this parser."
            )
        }
        // Scale by multiplierFactor
        distances(i)(j) = math.round(distDouble * multiplierFactor)
      }
    }

    // --- 4) Return the Problem ---
    Problem(nCities, distances, multiplierFactor)
  }

  // --------------------------------------------------------------------------
  // Private distance helpers for TSPLib
  // --------------------------------------------------------------------------

  /**
   * Standard Euclidean distance, but *not* scaled inside this method.
   * We return a Double so we can handle scaling outside.
   */
  private def euclidean2D(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val dx = x2 - x1
    val dy = y2 - y1
    math.sqrt(dx*dx + dy*dy)
  }

  /**
   * TSPLib "GEO" distance uses a great-circle approximation with
   * radius ~6378.388. The coordinates in the file are in "degrees.decimals"
   * where decimals * 100 => minutes.
   */
  private def geoDistance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    // Convert lat/long from "degree.decimal" to TSPLib's internal radians
    val (phi1, theta1) = toGeoRadians(lat1, lon1)
    val (phi2, theta2) = toGeoRadians(lat2, lon2)

    val R = 6378.388
    val q1 = cos(theta1 - theta2)
    val q2 = cos(phi1 - phi2)
    val q3 = cos(phi1 + phi2)

    val cosD = 0.5*((1.0 + q1)*q2 - (1.0 - q1)*q3)
    val dist = R * acos(math.max(math.min(cosD, 1.0), -1.0))
    dist + 1.0  // TSPLib typically adds 1 before rounding
  }

  /**
   * Convert from TSPLib "degree.decimal" notation
   * into the internal lat/long in radians that the "GEO" formula requires.
   *
   * e.g. 38.24 -> 38 deg + 0.24 * 100 minutes = 38 deg + 24 minutes
   * Then TSPLib says degrees + 5*minutes/3 = final degrees,
   * convert that final degrees => radians
   */
  private def toGeoRadians(latDegMin: Double, lonDegMin: Double): (Double, Double) = {
    def degMinToRadians(degMin: Double): Double = {
      val deg     = degMin.toInt
      val fraction= degMin - deg
      val minutes = fraction * 100.0
      val degreesPlus = deg + (5.0 * minutes / 3.0)
      math.Pi * degreesPlus / 180.0
    }

    val phi   = degMinToRadians(latDegMin)
    val theta = degMinToRadians(lonDegMin)
    (phi, theta)
  }

}
