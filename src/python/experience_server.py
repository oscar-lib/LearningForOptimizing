import socket
import logging
from message import Message


class ExperienceServer:
    def __init__(self, port: int = 5000):
        self.port = port
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.bind(("127.0.0.1", port))

    def run(self):
        logging.info(f"Starting server on port {self.port}")
        self.socket.listen(1)
        conn, addr = self.socket.accept()
        with conn:
            logging.info(f"Connected to {addr}")
            try:
                while True:
                    msg = Message.recv(conn)
                    print(msg)
            except ConnectionResetError:
                logging.info(f"Connection to {addr} closed")
                return
