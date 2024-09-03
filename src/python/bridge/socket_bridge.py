import socket

from .protocol import Message, Header

from .bridge import Bridge


class SocketBridge(Bridge):
    def __init__(self, port: int):
        super().__init__()
        self.port = port
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(("127.0.0.1", port))
        sock.listen(1)
        conn, addr = sock.accept()
        self.socket = conn

    def read(self, nbytes: int) -> bytes:
        return self.socket.recv(nbytes)

    def send(self, msg: Message):
        self.socket.send(msg.to_bytes())
