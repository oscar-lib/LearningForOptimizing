package bridge

import java.net.Socket

abstract class Body {
  def fromBytes(bytes: Array[Byte]): Body
  def toBytes(): Array[Byte]

  def send(socket: Socket): Unit = {
    val bytes = this.toBytes()
    println(s"Sending body: $bytes")
    socket.getOutputStream.write(bytes)
  }
}

class BodyV1(bytes: Array[Byte]) extends Body {
  override def fromBytes(bytes: Array[Byte]): Body = {
    new BodyV1(bytes)
  }

  override def toString(): String = {
    val s = new String(this.bytes)
    return s"BodyV1($s)"
  }

  override def toBytes(): Array[Byte] = {
    return this.bytes
  }
}
