package bridge

import java.net.Socket
import java.io.InputStream

object MessageType extends Enumeration {
  final val ACK           = Value(0)
  final val ERROR         = Value(1)
  final val STATIC_DATA   = Value(2)
  final val CURRENT_STATE = Value(3)

}

class Message(header: Header, body: Array[Byte]) {

  def toBytes(): Array[Byte] = {
    return header.toBytes() ++ body
  }

  def header(): Header = {
    return header
  }

  def msgType(): MessageType.Value = {
    return header.msgType()
  }

  override def toString(): String = {
    return s"Message(header=$header, body=$body)"
  }
}

object Message {
  def recv(input: InputStream): Message = {
    val header = Header.recv(input)
    val body   = new Array[Byte](header.payloadNbytes())
    input.read(body)
    return new Message(header, body)
  }

  def create(msgType: MessageType.Value, body: Array[Byte]): Message = {
    val header = new Header(1, body.length, msgType)
    return new Message(header, body)
  }
}
