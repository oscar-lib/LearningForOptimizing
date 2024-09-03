from .protocol import Message, Header
from abc import ABC, abstractmethod


class Bridge(ABC):
    def recv(self) -> Message:
        header_bytes = self.read(Header.SIZE)
        if len(header_bytes) == 0:
            raise ConnectionResetError("Connection closed by remote while waiting for header bytes")
        header = Header.from_bytes(header_bytes)
        if header.nbytes == 0:
            return Message(header, b"")
        data = self.read(header.nbytes)
        if len(data) == 0:
            raise ConnectionResetError(f"Connection closed by remote while waiting for payload data of {header}")
        return Message(header, data)

    @abstractmethod
    def read(self, nbytes: int) -> bytes:
        """Read a message from the input stream"""

    @abstractmethod
    def send(self, bytes: bytes):
        """Write a message to the output stream"""
