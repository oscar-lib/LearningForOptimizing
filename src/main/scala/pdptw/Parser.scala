package pdptw

import scala.annotation.tailrec
import scala.io.Source

object Parser {
  private val multFactor = 1000
	def apply(fileName: String): LiLimProblem = {

    val s = Source.fromFile(fileName)
    val lines = s.getLines()

    val Array(v, capacity, _) = lines.next().split("\\t\\s*").map(_.toInt)
    val Array(_,depotX,depotY) =  lines.next().split("\\t\\s*").map(_.toInt).take(3)
    val depot = LiLimDepot((depotX,depotY))
    val vehicles = List.fill(v)(LiLimVehicle(depot,capacity))


    @tailrec
    def extractNodesAndDemands(lines: Iterator[String], liLimNodes: List[LiLimNode] = List.empty, liLimDemands: List[LiLimDemand] = List.empty): (List[LiLimNode],List[LiLimDemand]) = {
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
        val node = LiLimNode(nodeId,(x,y),earliestArrivalTime,latestArrivalTime,duration)
        val demand =
          if(pickUp == 0) Some(LiLimDemand(nodeId,dropOff,quantity))
          else None
        extractNodesAndDemands(lines, liLimNodes :+ node, if(pickUp == 0) liLimDemands :+ demand.get else liLimDemands)
      } else {
        (liLimNodes,liLimDemands)
      }
    }

    val (nodes,demands) = extractNodesAndDemands(lines)

    s.close()

    LiLimProblem(vehicles, nodes, demands)
  }
}


case class LiLimProblem(
                       vehicles: List[LiLimVehicle],
                       nodes: List[LiLimNode],
                       demands: List[LiLimDemand]
                    )

case class LiLimDepot(positionXY: (Int,Int))
case class LiLimVehicle(depot: LiLimDepot, capacity: Int)
case class LiLimNode(nodeId: Int, positionXY: (Int,Int), earliestArrival: Int, latestArrival: Int, duration: Int)
case class LiLimDemand(fromNodeId: Int, toNodeId: Int, quantity: Int)
