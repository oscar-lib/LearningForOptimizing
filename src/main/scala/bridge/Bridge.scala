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
import combinator.RLAlgorithm

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

class NamedPipeBridge(input: InputStream, output: OutputStream, process: Option[Process])
    extends Bridge(input: InputStream, output: OutputStream) {
  def close(): Unit = {
    this.input.close()
    this.output.close()
    if (this.process.isEmpty) {
      return
    }
    process match {
      case Some(p) => {
        p.destroy()
        if (!p.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)) {
          p.destroyForcibly()
        }
      }
      case None => {}
    }
  }
}

object NamedPipeBridge {

  def apply(algo: RLAlgorithm.Value): NamedPipeBridge = {
    val DEBUG = true
    // Scala is responsible for creating the pipes.
    // Python is responsible for cleaning them up after the run.
    val id = if (DEBUG) { 0 }
    else { System.nanoTime() }
    // val id      = 0
    val pipeOut = s"pipes/s2p-$id"
    val pipeIn  = s"pipes/p2s-$id"
    new File("pipes").mkdirs();
    val pipe1 = new File(pipeOut)
    val pipe2 = new File(pipeIn)
    if (pipe1.exists()) {
      pipe1.delete()
    }
    if (pipe2.exists()) {
      pipe2.delete()
    }
    createFifo(pipeOut)
    createFifo(pipeIn)

    val builder =
      new ProcessBuilder(
        ".venv/bin/python",
        "src/python/main.py",
        "-c=pipe",
        s"-i=$pipeOut",
        s"-o=$pipeIn",
        s"-a=$algo",
        "--device=gpu"
      )
    val process = if (DEBUG) { None }
    else { Some(builder.start()) }
    val input  = new FileInputStream(new File(pipeIn))
    val output = new FileOutputStream(new File(pipeOut))
    new NamedPipeBridge(input, output, process)
  }

  def createFifo(path: String) = {
    val process = new ProcessBuilder("mkfifo", path).start()
    process.waitFor()
  }
}
