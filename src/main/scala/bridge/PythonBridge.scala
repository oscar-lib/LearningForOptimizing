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

  def sendStaticProblemData(problem: LiLimProblem, nActions: Int) = {
    val json = write(problem).dropRight(1) + s",\"nActions\":$nActions}"
    val msg  = Message.create(MessageType.STATIC_DATA, json.getBytes())
    this.output.write(msg.toBytes())
    val resp = Message.recv(this.input)
    if (resp.msgType() != MessageType.ACK) {
      println(resp.header(), resp.body())
      throw new Exception("Failed to send static problem data")
    }
  }

  def askInference(state: List[List[Int]]): List[Double] = {
    val jsonString = upickle.default.write(state)
    val message    = Message.create(MessageType.INFERENCE_REQ, jsonString.getBytes())
    this.output.write(message.toBytes())
    val response = Message.recv(this.input)
    if (response.msgType() != MessageType.INFERENCE_RSP) {
      throw new Exception("Failed to get inference response")
    }
    val qvalues = ujson.read(response.body()).arr.map(_.num).toList
    return qvalues
  }

  def sendReward(reward: Double) = {
    // this.output.write()
  }
}
