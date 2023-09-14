package pdptw

case class LiLimProblem(
                         vehicles: List[LiLimVehicle],
                         nodes: List[LiLimNode],
                         demands: List[LiLimCouple],
                         multFactor: Int
                       )

case class LiLimDepot(positionXY: (Int,Int))
case class LiLimVehicle(depot: LiLimDepot, capacity: Long)
case class LiLimNode(
                      nodeId: Int,
                      positionXY: (Int,Int),
                      earliestArrival: Int,
                      latestArrival: Int,
                      duration: Int,
                      quantity: Long
                    )
case class LiLimCouple(fromNodeId: Int, toNodeId: Int)
