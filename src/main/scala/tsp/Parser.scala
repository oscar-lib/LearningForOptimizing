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
   *  - Reads header (NAME, DIMENSION, EDGE_WEIGHT_TYPE, EDGE_WEIGHT_FORMAT, etc.)
   *  - If EDGE_WEIGHT_TYPE == EXPLICIT and EDGE_WEIGHT_FORMAT == FULL_MATRIX:
   *      -> Parses the full matrix after EDGE_WEIGHT_SECTION.
   *  - Otherwise, if e.g. EDGE_WEIGHT_TYPE == EUC_2D or GEO:
   *      -> Reads NODE_COORD_SECTION and computes distances.
   *  - Scales each distance by multiplierFactor for integer usage.
   */
  def parseTSPLibFile(file: File): Problem = {
    val lines = Source.fromFile(file).getLines().map(_.trim).toList
    if (lines.isEmpty)
      throw new IllegalArgumentException(s"File '${file.getName}' is empty or unreadable.")

    // --- 1) Extract header information ---
    val dimensionLine  = lines.find(_.toUpperCase.startsWith("DIMENSION")).getOrElse("")
    val ewtLine        = lines.find(_.toUpperCase.startsWith("EDGE_WEIGHT_TYPE")).getOrElse("")
    val ewfLine        = lines.find(_.toUpperCase.startsWith("EDGE_WEIGHT_FORMAT")).getOrElse("") // may be empty or missing
    val nCities = dimensionLine.split(":", 2).lift(1).map(_.trim.toInt)
      .getOrElse(throw new IllegalArgumentException("DIMENSION not found or invalid."))
    val edgeWeightType = ewtLine.split(":", 2).lift(1).map(_.trim.toUpperCase).getOrElse("EUC_2D")
    val edgeWeightFormat = ewfLine.split(":", 2).lift(1).map(_.trim.toUpperCase).getOrElse("")

    // Prepare the distance matrix
    val distances = Array.ofDim[Long](nCities, nCities)

    // Decide which logic to use:
    if (edgeWeightType == "EXPLICIT") {
      // 2A) EXPLICIT => parse from EDGE_WEIGHT_SECTION with one of the known formats
      fillMatrixExplicit(lines, nCities, edgeWeightFormat, distances)

    } else {
      // 2B) Coordinate-based => parse from NODE_COORD_SECTION (or similar), then compute distances
      fillMatrixCoordinates(lines, nCities, edgeWeightType, distances)
    }

    // --- 3) Return the resulting Problem ---
    Problem(nCities, distances, multiplierFactor)
  }

  // ==========================================================================
  // (A) EXPLICIT Distance Format
  // ==========================================================================
  private def fillMatrixExplicit(lines: List[String],
                                 nCities: Int,
                                 edgeWeightFormat: String,
                                 distances: Array[Array[Long]]): Unit = {
    // 1. Locate the line "EDGE_WEIGHT_SECTION"
    val idxSection = lines.indexWhere(_.toUpperCase == "EDGE_WEIGHT_SECTION")
    if (idxSection < 0) {
      throw new IllegalArgumentException(
        "Cannot find EDGE_WEIGHT_SECTION in an EXPLICIT TSP file."
      )
    }
    // 2. Gather all tokens after that line
    val tokens = lines
      .slice(idxSection + 1, lines.size)
      .flatMap(_.split("\\s+").filter(_.nonEmpty))

    // Different ways to interpret these tokens based on edgeWeightFormat
    edgeWeightFormat match {
      case "FULL_MATRIX" =>
        parseFullMatrix(tokens, nCities, distances)

      case "UPPER_ROW" =>
        parseUpperRow(tokens, nCities, distances, diagIncluded = false)

      case "LOWER_ROW" =>
        parseLowerRow(tokens, nCities, distances, diagIncluded = false)

      case "UPPER_DIAG_ROW" =>
        parseUpperRow(tokens, nCities, distances, diagIncluded = true)

      case "LOWER_DIAG_ROW" =>
        parseLowerRow(tokens, nCities, distances, diagIncluded = true)

      case "UPPER_COL" =>
        parseUpperCol(tokens, nCities, distances, diagIncluded = false)

      case "LOWER_COL" =>
        parseLowerCol(tokens, nCities, distances, diagIncluded = false)

      case "UPPER_DIAG_COL" =>
        parseUpperCol(tokens, nCities, distances, diagIncluded = true)

      case "LOWER_DIAG_COL" =>
        parseLowerCol(tokens, nCities, distances, diagIncluded = true)

      case "FUNCTION" =>
        // Weights are given by a function, so presumably the TSP file
        // would define some function or you compute them from coordinates.
        // Typically this implies a coordinate-based approach, not an explicit matrix.
        // But TSPLib allows "FUNCTION" under EXPLICIT. We'll just throw:
        throw new UnsupportedOperationException(
          "EXPLICIT / FUNCTION format not implemented in this parser."
        )

      case other =>
        throw new UnsupportedOperationException(
          s"EXPLICIT edge weight format '$other' is not supported in this parser."
        )
    }
  }

  /** Parse the entire matrix in row-major order: nCities * nCities tokens. */
  private def parseFullMatrix(tokens: List[String],
                              nCities: Int,
                              distances: Array[Array[Long]]): Unit = {
    val required = nCities * nCities
    if (tokens.size < required) {
      throw new IllegalArgumentException(
        s"FULL_MATRIX requires $required integers, found ${tokens.size}."
      )
    }

    val numbers = tokens.take(required).map(_.toLong) // no leftover parse
    var idx = 0
    for (i <- 0 until nCities; j <- 0 until nCities) {
      distances(i)(j) = numbers(idx) * multiplierFactor
      idx += 1
    }
  }

  /**
   * Parse a row-wise upper-triangular matrix (with or without diagonals).
   *
   * UPPER_ROW means the file lists the edges above the diagonal (i<j) in row order.
   * e.g. if diagIncluded = false, you skip diagonal, only upper part.
   * If diagIncluded = true, you include the diagonal as well in that sequence.
   */
  private def parseUpperRow(tokens: List[String],
                            nCities: Int,
                            distances: Array[Array[Long]],
                            diagIncluded: Boolean): Unit = {
    // Number of entries in the upper triangle
    // If diagIncluded, it's nCities*(nCities+1)/2, else it's nCities*(nCities-1)/2
    val required = if (diagIncluded) nCities*(nCities+1)/2 else nCities*(nCities-1)/2
    if (tokens.size < required) {
      throw new IllegalArgumentException(
        s"UPPER_ROW requires $required integers, found ${tokens.size}."
      )
    }
    val numbers = tokens.take(required).map(_.toLong)

    // Fill upper triangle
    var idx = 0
    for (i <- 0 until nCities) {
      // If diagIncluded => from j=i..(nCities-1)
      // else => from j=i+1..(nCities-1)
      val start = if (diagIncluded) i else i+1
      for (j <- start until nCities) {
        val value = numbers(idx) * multiplierFactor
        idx += 1
        distances(i)(j) = value
        distances(j)(i) = value
      }
    }
  }

  /** Same as parseUpperRow but for lower-triangular row-wise data. */
  private def parseLowerRow(tokens: List[String],
                            nCities: Int,
                            distances: Array[Array[Long]],
                            diagIncluded: Boolean): Unit = {
    val required = if (diagIncluded) nCities*(nCities+1)/2 else nCities*(nCities-1)/2
    if (tokens.size < required) {
      throw new IllegalArgumentException(
        s"LOWER_ROW requires $required integers, found ${tokens.size}."
      )
    }
    val numbers = tokens.take(required).map(_.toLong)

    var idx = 0
    for (i <- 0 until nCities) {
      // If diagIncluded => from j=0..i
      // else => from j=0..(i-1)
      val end = if (diagIncluded) i else i-1
      for (j <- 0 to end if j >= 0) {
        val value = numbers(idx) * multiplierFactor
        idx += 1
        distances(i)(j) = value
        distances(j)(i) = value
      }
    }
  }

  /**
   * Parse a column-wise upper-triangular format.
   * Similar logic but we iterate columns first.
   */
  private def parseUpperCol(tokens: List[String],
                            nCities: Int,
                            distances: Array[Array[Long]],
                            diagIncluded: Boolean): Unit = {
    val required = if (diagIncluded) nCities*(nCities+1)/2 else nCities*(nCities-1)/2
    if (tokens.size < required) {
      throw new IllegalArgumentException(
        s"UPPER_COL requires $required integers, found ${tokens.size}."
      )
    }
    val numbers = tokens.take(required).map(_.toLong)

    /*
      UPPER_COL means we list upper triangle by columns.
      For each column j, we have entries from row i=0..(j-1) or j.. if diagIncluded...
    */
    var idx = 0
    for (j <- 0 until nCities) {
      val start = if (diagIncluded) 0 else 0
      val end   = if (diagIncluded) j else j-1
      for (i <- start to end if i >= 0) {
        val value = numbers(idx) * multiplierFactor
        idx += 1
        distances(i)(j) = value
        distances(j)(i) = value
      }
    }
  }

  /** Parse column-wise lower-triangular formats. */
  private def parseLowerCol(tokens: List[String],
                            nCities: Int,
                            distances: Array[Array[Long]],
                            diagIncluded: Boolean): Unit = {
    val required = if (diagIncluded) nCities*(nCities+1)/2 else nCities*(nCities-1)/2
    if (tokens.size < required) {
      throw new IllegalArgumentException(
        s"LOWER_COL requires $required integers, found ${tokens.size}."
      )
    }
    val numbers = tokens.take(required).map(_.toLong)

    /*
      LOWER_COL => columns listing lower triangle.
      For column j, we read from row i=j..(nCities-1) or something similar.
      Actually, be mindful: "lower col" typically means that for each column j,
      we read i from j+1..(nCities-1) (or j.. if diagIncluded).
    */
    var idx = 0
    for (j <- 0 until nCities) {
      val start = if (diagIncluded) j else j+1
      for (i <- start until nCities if i < nCities) {
        val value = numbers(idx) * multiplierFactor
        idx += 1
        distances(i)(j) = value
        distances(j)(i) = value
      }
    }
  }

  // ==========================================================================
  // (B) Coordinate-based approach
  // ==========================================================================
  private def fillMatrixCoordinates(lines: List[String],
                                    nCities: Int,
                                    edgeWeightType: String,
                                    distances: Array[Array[Long]]): Unit = {

    // We look for "NODE_COORD_SECTION" or something similar.
    // If 2D, we expect lines with "id x y".
    // If 3D, we expect lines with "id x y z".
    // Some TSPLib files might omit "NODE_COORD_SECTION" if it's a function-based approach.
    val idxNodeSection = lines.indexWhere(_.toUpperCase == "NODE_COORD_SECTION")
    if (idxNodeSection < 0 && requiresCoordinates(edgeWeightType)) {
      throw new IllegalArgumentException(
        s"Cannot find NODE_COORD_SECTION for a coordinate-based TSP ($edgeWeightType)."
      )
    }

    // We'll assume 2D. If you want to handle 3D, check for "NODE_COORD_SECTION_3D" or parse differently.
    val coordsLines = lines.slice(idxNodeSection + 1, idxNodeSection + 1 + nCities)
    val coords = coordsLines.map { line =>
      val parts = line.split("\\s+").filter(_.nonEmpty)
      // For 2D, we assume: parts(0)=id, parts(1)=x, parts(2)=y
      // For 3D, you might want parts(3)=z
      if (parts.length < 3) {
        throw new IllegalArgumentException(s"Invalid node coordinate line: $line")
      }
      val x = parts(1).toDouble
      val y = parts(2).toDouble
      // If 3D, do val z = parts(3).toDouble
      // Return a (Double, Double, Double)? or separate approach for 3D
      (x, y)
    }.toArray

    // Fill distances
    for (i <- 0 until nCities; j <- 0 until nCities) {
      if (i == j) {
        distances(i)(j) = 0
      } else {
        val (x1, y1) = coords(i)
        val (x2, y2) = coords(j)
        val distDouble: Double = edgeWeightType match {
          case "EUC_2D"    => euclidean2D(x1, y1, x2, y2)
          case "CEIL_2D"   => ceil(euclidean2D(x1, y1, x2, y2))
          case "MAX_2D"    => maxDistance2D(x1, y1, x2, y2)
          case "MAN_2D"    => manhattan2D(x1, y1, x2, y2)
          case "GEO"       => geoDistance(x1, y1, x2, y2)
          case "ATT"       => attDistance(x1, y1, x2, y2)
          // Possibly 3D variants
          // e.g. "EUC_3D", "MAN_3D", "MAX_3D"
          // If you have 3D coords, parse them and implement a separate function

          case "EUC_3D" | "MAX_3D" | "MAN_3D" =>
            throw new UnsupportedOperationException(
              s"$edgeWeightType not fully implemented, needs 3D coordinate parsing."
            )

          case "XRAY1" | "XRAY2" | "SPECIAL" =>
            // specialized distance formula for X-ray or special TSP
            throw new UnsupportedOperationException(
              s"EDGE_WEIGHT_TYPE '$edgeWeightType' not implemented."
            )

          case other =>
            throw new UnsupportedOperationException(
              s"EDGE_WEIGHT_TYPE '$other' not supported in coordinate-based TSP."
            )
        }
        // scale it
        distances(i)(j) = math.round(distDouble * multiplierFactor)
      }
    }
  }

  /**
   * Utility to see if we need coordinates for a given edgeWeightType.
   * If it’s not EXPLICIT, it might require coordinate-based approach (or some specialized function).
   */
  private def requiresCoordinates(ewt: String): Boolean = {
    // EXPLICIT => no
    // Others => yes, unless function-based or specialized?
    // We'll just guess for demonstration. Adapt as you see fit.
    ewt match {
      case "EXPLICIT" => false
      case _          => true
    }
  }

  // ==========================================================================
  // Distance helpers
  // ==========================================================================

  /** Euclidean distance in 2D (returns a Double). */
  private def euclidean2D(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val dx = x2 - x1
    val dy = y2 - y1
    math.sqrt(dx*dx + dy*dy)
  }

  /** Manhattan distance in 2D. */
  private def manhattan2D(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    math.abs(x1 - x2) + math.abs(y1 - y2)
  }

  /** Maximum distance in 2D. */
  private def maxDistance2D(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    math.max(math.abs(x1 - x2), math.abs(y1 - y2))
  }

  /**
   * TSPLib "GEO": great-circle approximation using a formula with radius ~6378.388.
   * The file's lat/long coords are in "degree.decimal" (e.g., 38.24 => 38 degrees + 24 minutes).
   * Implementation is approximate here. Typically you do a TSPLib transformation to radians.
   */
  private def geoDistance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    // If the input is truly in TSPLib "degree + minutes" format, parse accordingly.
    // Here, for demonstration, we'll do a direct approximate approach.
    val R = 6378.388
    val dLat = toRadians(lat2 - lat1)
    val dLon = toRadians(lon2 - lon1)
    val a = sin(dLat/2)*sin(dLat/2) + cos(toRadians(lat1)) * cos(toRadians(lat2)) * sin(dLon/2)*sin(dLon/2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
    (R * c) + 1.0  // TSPLib often does dist + 1 before rounding
  }

  /** ATT distance function for att48, etc. (special TSPLib formula). Placeholder. */
  private def attDistance(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    // The TSPLib doc says:
    //   d = sqrt((x1 - x2)^2 / 10 + (y1 - y2)^2 / 10)
    //   Then round up. Something akin to that. Implementation is not official here.
    val xd = x1 - x2
    val yd = y1 - y2
    val rij = math.sqrt((xd*xd + yd*yd) / 10.0)
    // Round up to next integer, possibly. There's also a "special" rounding TSPLib does for ATT.
    math.ceil(rij)
  }

}
