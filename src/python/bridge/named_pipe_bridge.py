import os

from .bridge import Bridge


class NamedPipeBridge(Bridge):
    def __init__(self, input_filename: str, output_filename: str):
        super().__init__()
        self.input_filename = input_filename
        self.output_filename = output_filename
        self.output_stream = open(output_filename, "wb")
        self.intput_stream = open(input_filename, "rb")

    def read(self, nbytes: int) -> bytes:
        return self.intput_stream.read(nbytes)

    def send(self, bytes):
        bytes_written = self.output_stream.write(bytes)
        if bytes_written == 0:
            raise ConnectionResetError("Connection with remote closed")
        self.output_stream.flush()

    def __del__(self):
        self.close()

    def close(self):
        try:
            self.intput_stream.close()
        except AttributeError:
            pass
        try:
            os.remove(self.input_filename)
        except FileNotFoundError:
            pass
        try:
            self.output_stream.close()
        except AttributeError:
            pass
        try:
            os.remove(self.output_filename)
        except FileNotFoundError:
            pass
