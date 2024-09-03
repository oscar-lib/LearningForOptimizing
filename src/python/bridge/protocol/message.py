from dataclasses import dataclass
from enum import IntEnum
import logging
import socket
import json


class MessageType(IntEnum):
    ACK = 0
    ERROR = 1
    STATIC_DATA = 2
    ACTION_REQ = 3
    ACTION_RSP = 4
    REWARD = 5
    END_EPISODE = 6


@dataclass
class Header:
    version: int
    nbytes: int
    type: MessageType

    @classmethod
    def from_bytes(cls, data: bytes):
        assert len(data) == 12
        version = int.from_bytes(data[:4], byteorder="big")
        nbytes = int.from_bytes(data[4:8], byteorder="big")
        msg_type = MessageType(int.from_bytes(data[8:12], byteorder="big"))
        return Header(version, nbytes, msg_type)

    @classmethod
    def recv(cls, conn: socket.socket):
        bytes = conn.recv(12)
        if len(bytes) == 0:
            raise ConnectionResetError("Connection closed by remote while waiting for header bytes")
        header = cls.from_bytes(bytes)
        return header

    def to_bytes(self) -> bytes:
        return self.version.to_bytes(4, byteorder="big") + self.nbytes.to_bytes(4, byteorder="big") + self.type.to_bytes(4, byteorder="big")


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
        return Message(Header(1, len(reason), MessageType.ERROR), reason.encode())

    @staticmethod
    def ack() -> "Message":
        return Message(Header(1, 0, MessageType.ACK), b"")

    @staticmethod
    def inference_resp(action: int) -> "Message":
        int_bytes = action.to_bytes(4, byteorder="big")
        header = Header(1, len(int_bytes), MessageType.ACTION_RSP)
        return Message(header, int_bytes)
