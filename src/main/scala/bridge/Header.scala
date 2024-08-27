package bridge

import bridge.exceptions.FormatException
import bridge.exceptions.VersionException
import java.net.Socket
import java.nio.ByteBuffer
import java.nio.ByteOrder

abstract class Header(version: Int, payload_nbytes: Int) {
  def send(socket: Socket): Unit = {
    val buffer = ByteBuffer
      .allocate(Header.HEADER_SIZE)
      .order(ByteOrder.BIG_ENDIAN)
      .putInt(this.version)
      .putInt(this.payload_nbytes)
      .array()
    println(s"Sending header: ${buffer.mkString(", ")}")
    socket.getOutputStream.write(buffer)
  }
  def recvBody(socket: Socket): Body

  override def toString(): String = {
    return s"Header(version=$version, nbytes=$payload_nbytes)"
  }
}

// Companion object for Header (for static methods)
object Header {
  final val VERSION_SIZE = 4
  final val N_BYTES_SIZE = 4
  final val HEADER_SIZE  = VERSION_SIZE + N_BYTES_SIZE

  @throws[FormatException]
  @throws[VersionException]
  def fromBytes(bytes: Array[Byte]): Header = {
    if (bytes.length < HEADER_SIZE) {
      throw new FormatException(
        s"Header too short (min $HEADER_SIZE bytes for version number and payload size)"
      )
    }
    val version = ByteBuffer.wrap(bytes.slice(0, VERSION_SIZE)).order(ByteOrder.BIG_ENDIAN).getInt()
    val nbytes = ByteBuffer
      .wrap(bytes.slice(VERSION_SIZE, HEADER_SIZE))
      .order(ByteOrder.BIG_ENDIAN)
      .getInt()
    version match {
      case 1 => new HeaderV1(nbytes)
      case _ => throw new VersionException(1, version)
    }
  }

  def recv(socket: Socket): Header = {
    val headerBytes = new Array[Byte](VERSION_SIZE)
    socket.getInputStream.read(headerBytes)
    return Header.fromBytes(headerBytes)
  }
}

class HeaderV1(payload_nbytes: Int) extends Header(1, payload_nbytes) {

  override def recvBody(socket: Socket): Body = {
    val bodyBytes = new Array[Byte](this.payload_nbytes)
    socket.getInputStream.read(bodyBytes)
    return new BodyV1(bodyBytes)
  }
}
