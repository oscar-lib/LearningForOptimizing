package bridge

import java.net.Socket
import java.io.InputStream

object MessageType extends Enumeration {

  /** [bidirectional] Acknowledge message.
    */
  final val ACK   = Value(0)
  final val ERROR = Value(1)

  /** [scala to python] Send the static problem data at startup. Expects an ACK in response.
    */
  final val STATIC_DATA = Value(2)

  /** [scala to python] Send the state and ask for an inference */
  final val INFERENCE_REQ = Value(3)

  /** [python to scala] Response to an inference request with the qvalues */
  final val INFERENCE_RSP = Value(4)

  /** [scala to python] Send the reward signal for the previous transition
    */
  final val REWARD = Value(5)

  /** [scala to python] Notifies the end of an episode (reset in the search)
    */
  final val END_EPISODE = Value(6)

}

class Message(header: Header, body: Array[Byte]) {

  def toBytes(): Array[Byte] = {
    return header.toBytes() ++ body
  }

  def header(): Header = {
    return header
  }

  def body(): Array[Byte] = {
    return body
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

  def create(msgType: MessageType.Value, body: Array[Byte] = Array.empty): Message = {
    val header = new Header(1, body.length, msgType)
    return new Message(header, body)
  }
}
