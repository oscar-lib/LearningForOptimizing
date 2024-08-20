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

package pdptw

import java.io.File
import scala.annotation.tailrec
import scala.io.Source

object Parser {
  // The result of the benchmark is usually expressed in double.
  // We cannot deal with double within oscar therefore we multiply it by a factor and then divide it again.
  private val multiplierFactor = 1000
  def apply(file: File): LiLimProblem = {

    val s     = Source.fromFile(file)
    val lines = s.getLines()

    val Array(v, capacity, _)    = lines.next().split("\\t\\s*").map(_.toInt)
    val Array(_, depotX, depotY) = lines.next().split("\\t\\s*").map(_.toInt).take(3)
    val depot                    = LiLimDepot((multiplierFactor * depotX, multiplierFactor * depotY))
    val vehicles                 = List.fill(v)(LiLimVehicle(depot, capacity))

    @tailrec
    def extractNodesAndDemands(
      lines: Iterator[String],
      liLimNodes: List[LiLimNode] = List.empty,
      liLimDemands: List[LiLimCouple] = List.empty
    ): (List[LiLimNode], List[LiLimCouple]) = {
      if (lines.hasNext) {
        val next                = lines.next().split("\\t\\s*").map(_.toInt)
        val nodeId              = next(0)
        val x                   = multiplierFactor * next(1)
        val y                   = multiplierFactor * next(2)
        val quantity            = next(3)
        val earliestArrivalTime = multiplierFactor * next(4)
        val latestArrivalTime   = multiplierFactor * next(5)
        val duration            = multiplierFactor * next(6)
        val pickUp              = next(7)
        val dropOff             = next(8)
        val node =
          LiLimNode(nodeId, (x, y), earliestArrivalTime, latestArrivalTime, duration, quantity)
        val demand =
          if (pickUp == 0) List(LiLimCouple(nodeId, dropOff))
          else List.empty
        extractNodesAndDemands(lines, liLimNodes :+ node, liLimDemands ::: demand)
      } else {
        (liLimNodes, liLimDemands)
      }
    }

    val (nodes, demands) = extractNodesAndDemands(lines)

    s.close()

    LiLimProblem(vehicles, nodes, demands, multiplierFactor)
  }
}
