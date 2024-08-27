from dataclasses import dataclass
import socket


@dataclass
class HeaderV1:
    version: int
    nbytes: int

    @classmethod
    def from_bytes(cls, data: bytes):
        return HeaderV1(version=int.from_bytes(data[:4], byteorder="big"), nbytes=int.from_bytes(data[4:], byteorder="big"))

    @classmethod
    def recv(cls, conn: socket.socket):
        bytes = conn.recv(8)
        if len(bytes) == 0:
            raise ConnectionResetError()
        return cls.from_bytes(bytes)


@dataclass
class Message:
    header: HeaderV1
    body: bytes

    @classmethod
    def recv(cls, conn: socket.socket) -> "Message":
        header = HeaderV1.recv(conn)
        print(header)
        data = conn.recv(header.nbytes)
        if len(data) == 0:
            raise ConnectionResetError()
        return cls(header, data)
