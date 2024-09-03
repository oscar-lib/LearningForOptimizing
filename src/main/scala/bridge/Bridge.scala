package bridge

import java.net.Socket
import oscar.cbls.business.routing.model.VRP
import upickle.default._
import pdptw.LiLimNode
import pdptw.LiLimProblem
import pdptw.LiLimDepot
import pdptw.LiLimVehicle
import pdptw.LiLimCouple
import java.nio.ByteBuffer
import java.nio.ByteOrder
import org.apache.xpath.operations.Bool
import java.io.InputStream
import java.io.OutputStream
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import java.io.FileInputStream
import java.io.FileOutputStream

class Bridge(protected val input: InputStream, protected val output: OutputStream) {
  implicit val depRw: ReadWriter[LiLimDepot]   = macroRW
  implicit val vehRw: ReadWriter[LiLimVehicle] = macroRW
  implicit val nodRw: ReadWriter[LiLimNode]    = macroRW
  implicit val couRw: ReadWriter[LiLimCouple]  = macroRW
  implicit val pbRw: ReadWriter[LiLimProblem]  = macroRW

  def sendStaticProblemData(problem: LiLimProblem, nActions: Int): Unit = {
    val json = write(problem).dropRight(1) + s",\"nActions\":$nActions}"
    val msg  = Message.create(MessageType.STATIC_DATA, json.getBytes())
    this.output.write(msg.toBytes())
    val resp = Message.recv(this.input)
    if (resp.msgType() != MessageType.ACK) {
      println(resp.header(), resp.body())
      throw new Exception("Failed to send static problem data")
    }
  }
  def askAction(state: List[List[Int]], availabeActions: Array[Boolean]): Int = {
    val jsonState  = upickle.default.write(state)
    val jsonAvail  = upickle.default.write(availabeActions)
    val jsonString = s"""{"routes":$jsonState,"available":$jsonAvail}"""
    val message    = Message.create(MessageType.INFERENCE_REQ, jsonString.getBytes())
    val start      = System.currentTimeMillis()
    this.output.write(message.toBytes())
    val end = System.currentTimeMillis()
    println(s"Time to send message: ${end - start}")
    val response = Message.recv(this.input)
    if (response.msgType() != MessageType.INFERENCE_RSP) {
      throw new Exception("Failed to get inference response")
    }
    val body   = response.body()
    val action = ByteBuffer.wrap(body).order(ByteOrder.BIG_ENDIAN).getInt()
    action
  }
  def sendReward(reward: Double): Unit = {
    val rewardBytes =
      ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putFloat(reward.toFloat).array()
    val msg = Message.create(MessageType.REWARD, rewardBytes)
    this.output.write(msg.toBytes())
  }
  def sendEpisodeEnded(): Unit = {
    val msg   = Message.create(MessageType.END_EPISODE)
    val bytes = msg.toBytes()
    this.output.write(bytes)
  }

}

object Bridge {
  def socket(port: Int = 5000): Bridge = {
    val socket = new Socket("localhost", port)
    val input  = socket.getInputStream
    val output = socket.getOutputStream
    new Bridge(input, output)
  }

  def namedPipe(pipeName: String): Bridge = {
    import scala.io.Source
    val itr = Source.fromFile(pipeName)

    val input  = new java.io.FileInputStream(pipeName)
    val output = new java.io.FileOutputStream(pipeName)
    new Bridge(input, output)
  }
}

class SocketBridge(input: InputStream, output: OutputStream)
    extends Bridge(input: InputStream, output: OutputStream) {}

object SocketBridge {
  def apply(port: Int = 5555): SocketBridge = {
    val socket = new Socket("localhost", port)
    val input  = socket.getInputStream
    val output = socket.getOutputStream
    new SocketBridge(input, output)
  }
}

class NamedPipeBridge(input: InputStream, output: OutputStream)
    extends Bridge(input: InputStream, output: OutputStream) {}

object NamedPipeBridge {

  def apply(pipeNum: Int): NamedPipeBridge = {

    val pipeOut = s"pipes/$pipeNum-s2p"
    val pipeIn  = s"pipes/$pipeNum-p2s"
    new File("pipes").mkdirs();
    createFifo(pipeOut)
    createFifo(pipeIn)

    val process =
      new ProcessBuilder(
        ".venv/bin/python",
        "src/python/main.py",
        "-c",
        "pipe",
        "-i",
        pipeOut,
        "-o",
        pipeIn,
        "-a",
        "dqn"
      )
    // process.start()
    val input  = new FileInputStream(new File(pipeIn))
    val output = new FileOutputStream(new File(pipeOut))
    new NamedPipeBridge(input, output)
  }

  def createFifo(path: String) = {
    // val f = new File(path)
    // if (f.exists()) {
    //   f.delete()
    // }
    val process = new ProcessBuilder("mkfifo", path).start()
    process.waitFor()
  }
}
