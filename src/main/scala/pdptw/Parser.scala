package pdptw

import java.io.File
import scala.annotation.tailrec
import scala.io.Source

object Parser {
  private val multFactor = 1000
	def apply(file: File): LiLimProblem = {

    val s = Source.fromFile(file)
    val lines = s.getLines()

    val Array(v, capacity, _) = lines.next().split("\\t\\s*").map(_.toInt)
    val Array(_,depotX,depotY) =  lines.next().split("\\t\\s*").map(_.toInt).take(3)
    val depot = LiLimDepot((multFactor*depotX,multFactor*depotY))
    val vehicles = List.fill(v)(LiLimVehicle(depot,capacity))


    @tailrec
    def extractNodesAndDemands(lines: Iterator[String], liLimNodes: List[LiLimNode] = List.empty, liLimDemands: List[LiLimCouple] = List.empty): (List[LiLimNode],List[LiLimCouple]) = {
      if(lines.hasNext) {
        val next = lines.next().split("\\t\\s*").map(_.toInt)
        val nodeId = next(0)
        val x = multFactor * next(1)
        val y = multFactor * next(2)
        val quantity = next(3)
        val earliestArrivalTime = multFactor * next(4)
        val latestArrivalTime = multFactor * next(5)
        val duration = multFactor * next(6)
        val pickUp = next(7)
        val dropOff = next(8)
        val node = LiLimNode(nodeId,(x,y),earliestArrivalTime,latestArrivalTime,duration, quantity)
        val demand =
          if(pickUp == 0) List(LiLimCouple(nodeId,dropOff))
          else List.empty
        extractNodesAndDemands(lines, liLimNodes :+ node, liLimDemands ::: demand)
      } else {
        (liLimNodes,liLimDemands)
      }
    }

    val (nodes,demands) = extractNodesAndDemands(lines)

    s.close()

    LiLimProblem(vehicles, nodes, demands,multFactor)
  }
}


case class LiLimProblem(
                       vehicles: List[LiLimVehicle],
                       nodes: List[LiLimNode],
                       demands: List[LiLimCouple],
                       multFactor: Int
                    )

case class LiLimDepot(positionXY: (Int,Int))
case class LiLimVehicle(depot: LiLimDepot, capacity: Long)
case class LiLimNode(nodeId: Int, positionXY: (Int,Int), earliestArrival: Int, latestArrival: Int, duration: Int, quantity: Long)
case class LiLimCouple(fromNodeId: Int, toNodeId: Int)
