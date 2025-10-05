package bridge

import bridge.exceptions.FormatException
import bridge.exceptions.VersionException
import java.net.Socket
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.io.InputStream

class Header(payload_nbytes: Int, msg_type: MessageType.Value) {
  def toBytes(): Array[Byte] = {
    val id = this.msg_type.id
    return ByteBuffer
      .allocate(Header.HEADER_SIZE)
      .order(ByteOrder.BIG_ENDIAN)
      .putInt(this.payload_nbytes)
      .put(this.msg_type.id.toByte)
      .array()
  }

  def payloadNbytes(): Int = {
    return this.payload_nbytes
  }

  def msgType(): MessageType.Value = {
    return this.msg_type
  }

  override def toString(): String = {
    return s"Header(nbytes=$payload_nbytes, type=$msg_type)"
  }
}

// Companion object for Header (for static methods)
object Header {
  final val N_BYTES_SIZE = 4
  final val TYPE_SIZE    = 1
  final val HEADER_SIZE  = N_BYTES_SIZE + TYPE_SIZE

  @throws[FormatException]
  @throws[VersionException]
  def fromBytes(bytes: Array[Byte]): Header = {
    if (bytes.length < HEADER_SIZE) {
      throw new FormatException(
        s"Header too short (min $HEADER_SIZE bytes for version number and payload size)"
      )
    }
    val nbytes = ByteBuffer
      .wrap(bytes.slice(0, N_BYTES_SIZE))
      .order(ByteOrder.BIG_ENDIAN)
      .getInt()
    val msg_type = ByteBuffer
      .wrap(bytes.slice(N_BYTES_SIZE, HEADER_SIZE))
      .order(ByteOrder.BIG_ENDIAN)
      .get()
      .toInt

    return new Header(nbytes, MessageType(msg_type))
  }

  def recv(input: InputStream): Header = {
    val headerBytes = new Array[Byte](HEADER_SIZE)
    input.read(headerBytes)
    return Header.fromBytes(headerBytes)
  }
}
