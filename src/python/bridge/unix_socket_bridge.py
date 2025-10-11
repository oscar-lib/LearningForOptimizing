import logging
import socket

from .bridge import Bridge


class UnixSocketBridge(Bridge):
    def __init__(self, address: str):
        super().__init__()
        self.socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socket.connect(address)

    def read(self, nbytes: int) -> bytes:
        data = self.socket.recv(nbytes)
        if len(data) == 0:
            raise ConnectionResetError("Connection with remote closed")
        return data

    def send(self, bytes: bytes):
        self.socket.sendall(bytes)

    def __del__(self):
        try:
            self.socket.close()
        except AttributeError:
            pass
