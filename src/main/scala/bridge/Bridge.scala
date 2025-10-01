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
    val jsonState = problem.getJSONState()
    val jsonAvail = upickle.default.write(availabeActions)
    val jsonString =
      s"""{"state":$jsonState,"available":$jsonAvail}"""
    val message = Message.create(MessageType.INFERENCE_REQ, jsonString.getBytes())
    this.output.write(message.toBytes())
  }

  def sendReward(reward: Double, objValue: Double): Unit = {
    val rewardBytes =
      ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putFloat(reward.toFloat).array()
    val objBytes =
      ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putFloat(objValue.toFloat).array()
    val msg = Message.create(MessageType.REWARD, rewardBytes ++ objBytes)
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
    this.process match {
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
      val pathsStr = possiblePaths.map(_.toString()).mkString(", ")
      throw new Exception(f"Python executable not found in any of the expected paths ($pathsStr)")
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
    device: String,
    loadFrom: Option[String],
    saveTo: Option[String],
    training: Boolean,
    useTarget: Boolean
  ): NamedPipeBridge = {
    val pythonBinary       = this.findPythonPath()
    val pythonSrcDirectory = this.findPythonSourcesDirectory()

    val id = if (debug) { 0 }
    else { System.nanoTime() }
    // val id      = 0
    new File("pipes").mkdirs();
    val pipeOut = createFifoIfNotExists(s"pipes/s2p-$id")
    val pipeIn  = createFifoIfNotExists(s"pipes/p2s-$id")

    val process = if (!debug) {
      var command = new Array[String](0)
      command :+= pythonBinary.toString()
      command :+= pythonSrcDirectory.resolve("main.py").toString
      command :+= "--communication=pipe"
      command :+= f"-i=${pipeOut.getPath}"
      command :+= f"-o=${pipeIn.getPath}"
      command :+= f"-a=$algo"
      command :+= f"--device=${device}"
      command :+= f"--epsilon=$epsilon%.4f"
      command :+= f"--clipping=$clipping%.4f"
      command :+= f"--batch-size=$batchSize"
      if (useTarget) {
        command :+= f"--use-target"
      }
      if (ddqn) {
        command :+= f"--ddqn"
      }
      command :+= f"--lr=$lr%.4f"
      if (loadFrom.isDefined) {
        command :+= f"--load-from=${loadFrom.get}"
      }
      if (saveTo.isDefined) {
        command :+= f"--save-to=${saveTo.get}"
      }
      if (!training) {
        command :+= "--no-train"
      }
      val pb = new ProcessBuilder(command: _*)
      println(String.join(" ", pb.command()))
      val process = pb.start()
      println("Waiting 5 seconds for the process to start...")
      for (i <- 0 until 5) {
        print(f"Heartbeat ${i + 1}/5...")
        process.waitFor(1, java.util.concurrent.TimeUnit.SECONDS)
        if (!process.isAlive) {
          val msg = process.getErrorStream().readAllBytes().map(_.toChar).mkString
          throw new Exception(f"Python process did not start correctly: $msg")
        }
        println(" OK")
      }
      println("Python process successfully started.")
      Some(process)
    } else None

    val input  = new FileInputStream(pipeIn)
    val output = new FileOutputStream(pipeOut)
    new NamedPipeBridge(input, output, process)
  }

  def createFifoIfNotExists(path: String): File = {
    val pipe = new File(path)
    if (!pipe.exists()) {
      val process = new ProcessBuilder("mkfifo", path).start()
      process.waitFor()
    }
    pipe
  }
}
