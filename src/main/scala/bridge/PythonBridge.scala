package bridge

import java.net.Socket

class PythonBridge(port: Int = 5000) {
  val socket = new Socket("localhost", port)
  val input  = socket.getInputStream
  val output = socket.getOutputStream

  def send(data: Array[Byte]): Unit = {
    val header = new HeaderV1(data.length)
    val body   = new BodyV1(data)
    val msg    = new Message(header, body)
    msg.send(socket)
  }
}
