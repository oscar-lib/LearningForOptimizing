import socket
from typing import Callable
from .protocol import Message


class Bridge:
    def __init__(self, input_stream: socket.socket, output_stream: socket.socket, callback: Callable[[Message], None]):
        self.input = input_stream
        self.output = output_stream
        self.callback = callback

    def run(self):
        while True:
            msg = Message.recv(self.input)
            self.callback(msg)

    @staticmethod
    def from_socket(port: int, callback: Callable[[Message], None]):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(("127.0.0.1", port))
        return Bridge(sock, sock, callback)

    @staticmethod
    def from_named_pipe(callback: Callable[[Message], None]):
        pipe_in = open("/tmp/ipc-scala2py", "rb")
        pipe_out = open("/tmp/ipc-py2scala", "wb")
