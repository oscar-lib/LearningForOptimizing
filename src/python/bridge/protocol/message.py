from dataclasses import dataclass
from enum import IntEnum
from typing import ClassVar
import socket


class MessageType(IntEnum):
    ACK = 0
    ERROR = 1
    STATIC_DATA_PDPTW = 2
    ACTION_REQ = 3
    ACTION_RSP = 4
    REWARD = 5
    END_EPISODE = 6
    STATIC_DATA_CSP = 7
    TRANSITION = 8
    STATIC_DATA_TSP = 9


@dataclass
class Header:
    nbytes: int
    type: MessageType

    SIZE: ClassVar[int] = 5

    @classmethod
    def from_bytes(cls, data: bytes):
        assert len(data) == Header.SIZE
        nbytes = int.from_bytes(data[:4], byteorder="big")
        msg_type = MessageType(int.from_bytes(data[4:8], byteorder="big"))
        return Header(nbytes, msg_type)

    @classmethod
    def recv(cls, conn: socket.socket):
        bytes = conn.recv(Header.SIZE)
        if len(bytes) == 0:
            raise ConnectionResetError("Connection closed by remote while waiting for header bytes")
        header = cls.from_bytes(bytes)
        return header

    def to_bytes(self) -> bytes:
        return self.nbytes.to_bytes(4, byteorder="big") + self.type.to_bytes(1, byteorder="big")


@dataclass
class Message:
    header: Header
    body: bytes

    @classmethod
    def recv(cls, conn: socket.socket) -> "Message":
        header = Header.recv(conn)
        if header.nbytes > 0:
            data = conn.recv(header.nbytes)
            if len(data) == 0:
                raise ConnectionResetError(f"Connection closed by remote while waiting for payload data of {header}")
            return cls(header, data)
        return cls(header, b"")

    def to_bytes(self) -> bytes:
        return self.header.to_bytes() + self.body

    @property
    def type(self):
        return self.header.type

    @staticmethod
    def error(reason: str) -> "Message":
        return Message(Header(len(reason), MessageType.ERROR), reason.encode())

    @staticmethod
    def ack() -> "Message":
        return Message(Header(0, MessageType.ACK), b"")

    @staticmethod
    def inference_resp(action: int) -> "Message":
        int_bytes = action.to_bytes(4, byteorder="big")
        header = Header(len(int_bytes), MessageType.ACTION_RSP)
        return Message(header, int_bytes)
