import socket
import logging
from message import Message, MessageType
from problem import Problem


class ExperienceServer:
    def __init__(self, port: int = 5000):
        self.port = port
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.bind(("127.0.0.1", port))

    def run(self):
        logging.info(f"Starting server on port {self.port}")
        self.socket.listen(1)
        try:
            while True:
                self.handle_client()
        except KeyboardInterrupt:
            pass
        logging.info("Shutting down server")

    def handle_client(self):
        conn, addr = self.socket.accept()
        with conn:
            logging.info(f"Connected to {addr}")
            try:
                logging.info("Waiting for static problem data")
                msg = Message.recv(conn)
                if msg.type != MessageType.STATIC_DATA:
                    logging.error("Expected static data")
                    conn.send(Message.error("Expected to get problem data").to_bytes())
                    return
                problem = Problem.parse(msg.body)
                conn.send(Message.ack().to_bytes())
                while True:
                    msg = Message.recv(conn)
                    print(msg)
            except ConnectionResetError:
                logging.info(f"Connection with {addr} closed")
                return
