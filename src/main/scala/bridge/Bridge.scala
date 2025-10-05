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
import java.net.UnixDomainSocketAddress
import java.nio.channels.SocketChannel
import java.nio.channels.ServerSocketChannel
import java.net.StandardProtocolFamily

abstract class Bridge(
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
  noTarget: Boolean,
  logdir: Option[String],
  memorySize: Int,
  seed: Int
) {

  def recv(): Message
  def send(msg: Message): Unit
  def completeCommand(command: Array[String]): Array[String]

  val process = this.startSubprocess(false)

  def startSubprocess(withHeartbeat: Boolean): Option[Process] = {
    if (!debug) {
      val command = this.makeCommand()
      val pb      = new ProcessBuilder(command: _*)
      println(String.join(" ", pb.command()))
      val process = pb.start()
      if (withHeartbeat) {
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
      }
      Some(process)
    } else None
  }

  def makeCommand(): Array[String] = {
    val pythonBinary       = Bridge.findPythonPath()
    val pythonSrcDirectory = Bridge.findPythonSourcesDirectory()
    var command            = new Array[String](0)
    command :+= pythonBinary.toString()
    command :+= pythonSrcDirectory.resolve("main.py").toString
    command :+= f"-a=${this.algo}"
    command :+= f"--device=${this.device}"
    command :+= f"--epsilon=${this.epsilon}%.4f"
    command :+= f"--clipping=${this.clipping}%.4f"
    command :+= f"--batch-size=${this.batchSize}"
    command :+= f"--seed=${this.seed}"
    command :+= "--disable-training-logs"
    command :+= f"--memory-size=${this.memorySize}"
    if (noTarget) {
      command :+= f"--no-target"
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
    if (logdir.isDefined) {
      command :+= f"--logdir=${logdir.get}"
    }
    this.completeCommand(command) // allow subclasses to modify the launch command
  }
  // protected val process =

  def sendStaticProblemData(problem: SerializableModel, nActions: Int): Unit = {
    val data = problem.getJSONStaticProblemData()
    val json = s"""{"problem":$data,"nActions":$nActions}"""
    val msg  = Message.create(problem.getProblemCode(), json.getBytes());
    this.send(msg)
    // this.output.write(msg.toBytes())
    val resp = this.recv()
    if (resp.msgType() != MessageType.ACK) {
      println(resp.header(), resp.body())
      throw new Exception("Failed to send static problem data")
    }
  }

  def askAction(problem: SerializableModel, availabeActions: Array[Boolean]): Int = {
    this.sendActionData(problem, availabeActions)
    val response = this.recv()
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
    this.send(Message.create(MessageType.INFERENCE_REQ, jsonString.getBytes()))
  }

  def sendReward(reward: Double, objValue: Double): Unit = {
    val rewardBytes =
      ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putFloat(reward.toFloat).array()
    val objBytes =
      ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putFloat(objValue.toFloat).array()
    val msg = Message.create(MessageType.REWARD, rewardBytes ++ objBytes)
    this.send(msg)
  }
  def sendEpisodeEnded(): Unit = {
    val msg = Message.create(MessageType.END_EPISODE)
    this.send(msg)
  }

  def sendTransition(state: String, action: Int, nextState: String, reward: Double): Unit = {
    val json = s"""{"state":$state,"action":$action,"nextState":$nextState,"reward":$reward}"""
    val msg  = Message.create(MessageType.TRANSITION, json.getBytes())
    this.send(msg)
  }

  def close(): Unit = {
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

object Bridge {
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

  // def namedPipe(
  //   algo: RLAlgorithm.Value,
  //   debug: Boolean,
  //   batchSize: Int,
  //   epsilon: Double,
  //   clipping: Double,
  //   lr: Double,
  //   ddqn: Boolean,
  //   device: String,
  //   loadFrom: Option[String],
  //   saveTo: Option[String],
  //   training: Boolean,
  //   noTarget: Boolean,
  //   logdir: Option[String],
  //   seed: Int
  // ): Bridge = {
  //   // Create pipes
  //   val id = if (debug) { 0 }
  //   else { System.nanoTime() }
  //   new File("/tmp/pipes").mkdirs();
  //   val pipeOut = createFifoIfNotExists(s"/tmp/pipes/s2p-$id")
  //   val pipeIn  = createFifoIfNotExists(s"/tmp/pipes/p2s-$id")

  //   // Start subprocess if not in debug mode
  //   val process = if (!debug) {
  //     var command = makePartialCommand(
  //       algo,
  //       debug,
  //       batchSize,
  //       epsilon,
  //       clipping,
  //       lr,
  //       ddqn,
  //       device,
  //       loadFrom,
  //       saveTo,
  //       training,
  //       noTarget,
  //       logdir,
  //       seed
  //     )
  //     command :+= "--communication=pipe"
  //     command :+= f"-i=${pipeOut.getPath}"
  //     command :+= f"-o=${pipeIn.getPath}"
  //     Some(startProcess(command))
  //   } else None

  //   val input  = new FileInputStream(pipeIn)
  //   val output = new FileOutputStream(pipeOut)
  //   new Bridge(input, output, process)
  // }

  // def unixSocket(
  //   algo: RLAlgorithm.Value,
  //   debug: Boolean,
  //   batchSize: Int,
  //   epsilon: Double,
  //   clipping: Double,
  //   lr: Double,
  //   ddqn: Boolean,
  //   device: String,
  //   loadFrom: Option[String],
  //   saveTo: Option[String],
  //   training: Boolean,
  //   noTarget: Boolean,
  //   logdir: Option[String],
  //   seed: Int
  // ): Bridge = {
  //   val id = if (debug) { 0 }
  //   else { System.nanoTime() }
  //   new File("/tmp/pipes").mkdirs();
  //   val address = UnixDomainSocketAddress.of(s"/tmp/pipes/unix-socket-$id")
  //   // val server        = ServerSocketChannel.open();
  //   val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX);
  //   server.bind(address);
  //   println(s"Listening on ${address.toString}")

  //   val process = if (!debug) {
  //     var command = makePartialCommand(
  //       algo,
  //       debug,
  //       batchSize,
  //       epsilon,
  //       clipping,
  //       lr,
  //       ddqn,
  //       device,
  //       loadFrom,
  //       saveTo,
  //       training,
  //       noTarget,
  //       logdir,
  //       seed
  //     )
  //     command :+= "--communication=unix-socket"
  //     command :+= s"-i=${address.toString}"
  //     Some(startProcess(command))
  //   } else None

  //   val channel = server.accept()
  //   val socket  = channel.socket() // UNSUPPORTED Operation Exception
  //   val input   = socket.getInputStream
  //   val output  = socket.getOutputStream
  //   new Bridge(input, output, process)
  // }
}

class NamedPipeBridge(
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
  noTarget: Boolean,
  memorySize: Int,
  logdir: Option[String],
  seed: Int
) extends {
      // Early initialization block before the superclass constructor
      val _unused = new File("/tmp/pipes").mkdirs();
      val id = if (debug) { 0 }
      else { System.nanoTime() }
      val pipeOut = NamedPipeBridge.createFifoIfNotExists(s"/tmp/pipes/s2p-${id}")
      val pipeIn  = NamedPipeBridge.createFifoIfNotExists(s"/tmp/pipes/p2s-${id}")
    }
    with Bridge(
      algo,
      debug,
      batchSize,
      epsilon,
      clipping,
      lr,
      ddqn,
      device,
      loadFrom,
      saveTo,
      training,
      noTarget,
      logdir,
      memorySize = memorySize,
      seed = seed
    ) {

  val input  = new FileInputStream(this.pipeIn)
  val output = new FileOutputStream(this.pipeOut)

  def completeCommand(command: Array[String]): Array[String] = {
    var cmd = command
    cmd :+= "--communication=pipe"
    cmd :+= f"-i=${this.pipeOut.getPath}"
    cmd :+= f"-o=${this.pipeIn.getPath}"
    cmd
  }

  def recv(): Message = {
    val header = Header.recv(this.input)
    val body   = new Array[Byte](header.payloadNbytes())
    this.input.read(body)
    return new Message(header, body)
  }

  def send(msg: Message): Unit = {
    this.output.write(msg.toBytes())
  }
}

object NamedPipeBridge {
  def createFifoIfNotExists(path: String): File = {
    val pipe = new File(path)
    if (!pipe.exists()) {
      val process = new ProcessBuilder("mkfifo", path).start()
      process.waitFor()
    }
    pipe
  }
}

class UnixPipeBridge(
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
  noTarget: Boolean,
  logdir: Option[String],
  memorySize: Int,
  seed: Int
) extends {
      val id = if (debug) { 0 }
      else { System.nanoTime() }
      val address = UnixDomainSocketAddress.of(s"/tmp/pipes/unix-socket-$id")
      val _unused = {
        new File("/tmp/pipes").mkdirs();
        val file = new File(address.toString())
        if (file.exists()) {
          file.delete()
        }
      }
      val server = {
        val s = ServerSocketChannel.open(StandardProtocolFamily.UNIX);
        s.bind(address);
        s
      }

    }
    with Bridge(
      algo,
      debug,
      batchSize,
      epsilon,
      clipping,
      lr,
      ddqn,
      device,
      loadFrom,
      saveTo,
      training,
      noTarget,
      logdir,
      memorySize = memorySize,
      seed = seed
    ) {

  println(s"Listening on ${this.address.toString}")
  val socket = this.server.accept()

  override def completeCommand(command: Array[String]): Array[String] = {
    var cmd = command
    cmd :+= "--communication=unix-socket"
    cmd :+= s"-i=${this.address.toString}"
    cmd
  }

  override def recv(): Message = {
    val headerBuffer = ByteBuffer.allocate(Header.HEADER_SIZE);
    this.socket.read(headerBuffer);
    headerBuffer.flip();
    val header     = Header.fromBytes(headerBuffer.array())
    val bodyNBytes = header.payloadNbytes()
    val body = if (bodyNBytes > 0) {
      val bodyBuffer = ByteBuffer.allocate(bodyNBytes);
      this.socket.read(bodyBuffer);
      bodyBuffer.array()
    } else {
      Array.empty[Byte]
    }
    new Message(header, body)
  }

  def send(msg: Message): Unit = {
    val bytes  = msg.toBytes()
    val buffer = ByteBuffer.allocate(bytes.length);
    buffer.put(bytes);
    buffer.flip();
    while (buffer.hasRemaining()) {
      this.socket.write(buffer);
    }
  }
}
