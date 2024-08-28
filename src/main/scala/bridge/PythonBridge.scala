package bridge

import java.net.Socket
import oscar.cbls.business.routing.model.VRP
import upickle.default._
import pdptw.LiLimNode
import pdptw.LiLimProblem
import pdptw.LiLimDepot
import pdptw.LiLimVehicle
import pdptw.LiLimCouple

class PythonBridge(port: Int = 5000) {
  val socket                                   = new Socket("localhost", port)
  val input                                    = socket.getInputStream
  val output                                   = socket.getOutputStream
  implicit val depRw: ReadWriter[LiLimDepot]   = macroRW
  implicit val vehRw: ReadWriter[LiLimVehicle] = macroRW
  implicit val nodRw: ReadWriter[LiLimNode]    = macroRW
  implicit val couRw: ReadWriter[LiLimCouple]  = macroRW
  implicit val pbRw: ReadWriter[LiLimProblem]  = macroRW

  def sendStaticProblemData(problem: LiLimProblem) = {
    val json = write(problem)
    val msg  = Message.create(MessageType.STATIC_DATA, json.getBytes())
    this.output.write(msg.toBytes())
    val resp = Message.recv(this.input)
    if (resp.msgType() != MessageType.ACK) {
      throw new Exception("Failed to send static problem data")
    }
  }

  def notifyTransition() = {}
}
