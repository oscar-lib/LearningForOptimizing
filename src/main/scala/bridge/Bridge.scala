package bridge

import combinator.RLAlgorithm
import csp.CarSeqConf
import csp.CarSeqProblem
import oscar.cbls.business.routing.model.VRP
import pdptw.LiLimCouple
import pdptw.LiLimDepot
import pdptw.LiLimNode
import pdptw.LiLimProblem
import pdptw.LiLimVehicle
import upickle.default._

import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileReader
import java.io.InputStream
import java.io.OutputStream
import java.net.Socket
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Paths
import java.nio.file.Path

class Bridge(protected val input: InputStream, protected val output: OutputStream) {

  def sendStaticProblemData(problem: SerializableModel, nActions: Int): Unit = {
    val data = problem.getJSONStaticProblemData()
    val json = s"""{"problem":$data,"nActions":$nActions}"""
    val msg  = Message.create(problem.getProblemCode(), json.getBytes());
    this.output.write(msg.toBytes())
    val resp = Message.recv(this.input)
    if (resp.msgType() != MessageType.ACK) {
      println(resp.header(), resp.body())
      throw new Exception("Failed to send static problem data")
    }
  }

  def askAction(problem: SerializableModel, availabeActions: Array[Boolean]): Int = {
    this.sendActionData(problem, availabeActions)
    val response = Message.recv(this.input)
    if (response.msgType() != MessageType.INFERENCE_RSP) {
      throw new Exception("Failed to get inference response")
    }
    val body   = response.body()
    val action = ByteBuffer.wrap(body).order(ByteOrder.BIG_ENDIAN).getInt()
    action
  }

  def sendActionData(problem: SerializableModel, availabeActions: Array[Boolean]) = {
    val jsonState  = problem.getJSONState()
    val jsonAvail  = upickle.default.write(availabeActions)
    val jsonString = s"""{"state":$jsonState,"available":$jsonAvail}"""
    val message    = Message.create(MessageType.INFERENCE_REQ, jsonString.getBytes())
    this.output.write(message.toBytes())
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

  def sendTransition(state: String, action: Int, nextState: String, reward: Double): Unit = {
    val json = s"""{"state":$state,"action":$action,"nextState":$nextState,"reward":$reward}"""
    val msg  = Message.create(MessageType.TRANSITION, json.getBytes())
    this.output.write(msg.toBytes())
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

  def findPythonPath(): Path = {
    val possiblePaths = Array(
      Paths.get(".venv/bin/python"),
      Paths.get(".env/bin/python"),
      Paths.get("venv/bin/python"),
      Paths.get("env/bin/python"),
      Paths.get("python"),
      Paths.get("python3")
    )
    possiblePaths.find(_.toFile.exists()).getOrElse {
      throw new Exception("Python executable not found")
    }
  }

  def findPythonSourcesDirectory(): Path = {
    val possiblePaths = Array(Paths.get("./src/python"), Paths.get("../src/python"))

    possiblePaths.find(_.toFile.exists()).getOrElse {
      throw new Exception("Python sources path not found")
    }
  }

  def apply(
    algo: RLAlgorithm.Value,
    debug: Boolean,
    batchSize: Int,
    epsilon: Double,
    clipping: Double,
    lr: Double,
    ddqn: Boolean,
    device: String
  ): NamedPipeBridge = {
    // Scala is responsible for creating the pipes.
    // Python is responsible for cleaning them up after the run.

    val pythonBinary       = this.findPythonPath()
    val pythonSrcDirectory = this.findPythonSourcesDirectory()

    val id = if (debug) { 0 }
    else { System.nanoTime() }
    // val id      = 0
    val pipeOut = s"pipes/s2p-$id"
    val pipeIn  = s"pipes/p2s-$id"
    new File("pipes").mkdirs();
    createFifo(pipeOut)
    createFifo(pipeIn)

    val process = if (!debug) {
      val pb = new ProcessBuilder(
        pythonBinary.toString(),
        pythonSrcDirectory.resolve("main.py").toString,
        "--communication=pipe",
        s"-i=$pipeOut",
        s"-o=$pipeIn",
        s"-a=$algo",
        s"--device=gpu",
        f"--epsilon=$epsilon%.4f",
        f"--clipping=$clipping%.4f",
        s"--batch-size=$batchSize",
        s"--ddqn=$ddqn",
        f"--lr=$lr%.4f"
      );
      println(String.join(" ", pb.command()))
      Some(pb.start())
    } else None
    println("Python process started with PID" + process.map(_.pid()).getOrElse("N/A"))
    process match {
      case Some(p) => {
        println("Waiting 5 seconds for the process to start...")
        p.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)
        if (!p.isAlive) {
          val msg = p.getErrorStream().readAllBytes().map(_.toChar).mkString
          println("Python process error output: " + msg)
          throw new Exception(
            "Python process did not start correctly. Check the logs. Error: " + msg
          )
        }
        println("Python process is alive.")
      }
      case None => true // No process to wait for in debug mode
    }

    val input  = new FileInputStream(new File(pipeIn))
    val output = new FileOutputStream(new File(pipeOut))
    new NamedPipeBridge(input, output, process)
  }

  def createFifo(path: String) = {
    val pipe1 = new File(path)
    if (!pipe1.exists()) {
      val process = new ProcessBuilder("mkfifo", path).start()
      process.waitFor()
    }
  }
}
