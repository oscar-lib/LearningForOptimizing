package bridge

import bridge.exceptions.FormatException
import bridge.exceptions.VersionException
import java.net.Socket
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.io.InputStream

class Header(version: Int, payload_nbytes: Int, msg_type: MessageType.Value) {
  def toBytes(): Array[Byte] = {
    val id = this.msg_type.id
    return ByteBuffer
      .allocate(Header.HEADER_SIZE)
      .order(ByteOrder.BIG_ENDIAN)
      .putInt(this.version)
      .putInt(this.payload_nbytes)
      .putInt(this.msg_type.id)
      .array()
  }

  def payloadNbytes(): Int = {
    return this.payload_nbytes
  }

  def msgType(): MessageType.Value = {
    return this.msg_type
  }

  override def toString(): String = {
    return s"Header(version=$version, nbytes=$payload_nbytes, type=$msg_type)"
  }
}

// Companion object for Header (for static methods)
object Header {
  final val VERSION_SIZE = 4
  final val N_BYTES_SIZE = 4
  final val TYPE_SIZE    = 4
  final val HEADER_SIZE  = VERSION_SIZE + N_BYTES_SIZE + TYPE_SIZE

  @throws[FormatException]
  @throws[VersionException]
  def fromBytes(bytes: Array[Byte]): Header = {
    if (bytes.length < HEADER_SIZE) {
      throw new FormatException(
        s"Header too short (min $HEADER_SIZE bytes for version number and payload size)"
      )
    }
    val version = ByteBuffer
      .wrap(bytes.slice(0, VERSION_SIZE))
      .order(ByteOrder.BIG_ENDIAN)
      .getInt()
    if (version != 1) {
      throw new VersionException(1, version)
    }

    val nbytes = ByteBuffer
      .wrap(bytes.slice(VERSION_SIZE, HEADER_SIZE))
      .order(ByteOrder.BIG_ENDIAN)
      .getInt()
    val msg_type = ByteBuffer
      .wrap(bytes.slice(VERSION_SIZE + N_BYTES_SIZE, HEADER_SIZE))
      .order(ByteOrder.BIG_ENDIAN)
      .getInt()

    return new Header(version, nbytes, MessageType(msg_type))
  }

  def recv(input: InputStream): Header = {
    val headerBytes = new Array[Byte](VERSION_SIZE)
    input.read(headerBytes)
    return Header.fromBytes(headerBytes)
  }
}
