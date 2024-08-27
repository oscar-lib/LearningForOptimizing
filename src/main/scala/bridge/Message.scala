package bridge

import java.net.Socket

class Message(header: Header, body: Body) {

  def send(socket: Socket): Unit = {
    this.header.send(socket)
    this.body.send(socket)
  }

  override def toString(): String = {
    return s"Message(header=$header, body=$body)"
  }
}

object Message {
  def recv(socket: Socket): Message = {
    val header = Header.recv(socket)
    val body   = header.recvBody(socket)
    return new Message(header, body)
  }
}
