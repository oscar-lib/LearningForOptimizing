import os

from .bridge import Bridge


class NamedPipeBridge(Bridge):
    def __init__(self, input_filename: str, output_filename: str):
        super().__init__()
        self._make_fifo(input_filename)
        self.input_filename = input_filename
        self._make_fifo(output_filename)
        self.output_filename = output_filename
        self.intput_stream = open(input_filename, "rb")
        self.output_stream = open(output_filename, "wb")

    def read(self, nbytes: int) -> bytes:
        return self.intput_stream.read(nbytes)

    def send(self, bytes):
        self.output_stream.write(bytes)
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

    def _make_fifo(self, filename: str):
        try:
            os.mkfifo(filename)
        except FileExistsError:
            # print("Named pipe already exists!")
            pass
        except OSError as e:
            print(f"Named pipe creation failed: {e}")
