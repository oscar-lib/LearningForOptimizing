import json
import logging
import socket
import time

import numpy as np
from icecream import ic
from message import Message, MessageType
from problem import Problem


class ExperienceServer:
    def __init__(self, port: int = 5000):
        self.port = port
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
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
                req = Message.recv(conn)
                if req.type != MessageType.STATIC_DATA:
                    error = f"Expected message of type {MessageType.STATIC_DATA} from the client, got {req.type}"
                    logging.error(error)
                    conn.send(Message.error(error).to_bytes())
                    return
                problem = Problem.parse(req.body)
                conn.send(Message.ack().to_bytes())
                while True:
                    logging.info("Waiting for experience data")
                    req = Message.recv(conn)
                    match req.type:
                        case MessageType.INFERENCE_REQ:
                            resp = self.handle_inference_request(problem, req.body)
                        case MessageType.REWARD:
                            resp = self.handle_reward(problem, req.body)
                        case MessageType.END_EPISODE:
                            resp = self.handle_end_episode(problem, req.body)
                        case other:
                            resp = Message.error(f"Unexpected message type: {other}")
                    conn.send(resp.to_bytes())
            except ConnectionResetError:
                logging.info(f"Connection with {addr} closed")
                return

    def handle_inference_request(self, problem: Problem, body: bytes):
        routes = json.loads(body)
        problem.build_agent_input(routes)
        resp = Message.inference_resp(np.random.random(problem.n_actions).tolist())
        return resp

    def handle_reward(self, problem: Problem, body: bytes):
        logging.debug(body)
        return Message.ack()

    def handle_end_episode(self, problem: Problem, body: bytes):
        logging.debug(body)
        return Message.ack()
