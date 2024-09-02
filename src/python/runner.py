import socket
from datetime import datetime

from dqn import DQN
from loggers import Logger
from message import Message, MessageType
from optimenv import EpisodeEndException, OptimEnv
from problem import Problem


class Runner:
    def __init__(self, port: int = 5000):
        self.port = port
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(("127.0.0.1", port))
        self.logger = Logger(datetime.now().strftime("logs/%Y-%m-%d_%H-%M-%S"), csv=True, wandb=True)

    def run(self):
        self.logger.info(f"Starting server on port {self.port}")
        self.socket.listen(1)
        try:
            while True:
                self.handle_client()
        except KeyboardInterrupt:
            pass
        self.logger.info("Shutting down server")

    def handle_client(self):
        conn, addr = self.socket.accept()
        with conn:
            self.logger.info(f"Connected to {addr}")
            try:
                req = Message.recv(conn)
                if req.type != MessageType.STATIC_DATA:
                    error = f"Expected message of type {MessageType.STATIC_DATA} from the client, got {req.type}"
                    self.logger.error(error)
                    conn.send(Message.error(error).to_bytes())
                    return
                problem = Problem.parse(req.body)
                conn.send(Message.ack().to_bytes())
                agent = DQN.default(problem)
                env = OptimEnv(problem, conn)
                t = 0
                while True:
                    obs = env.reset()
                    try:
                        while True:
                            t += 1
                            action = agent.select_action(obs)
                            next_obs, reward = env.step(action)
                            logs = agent.learn(t, obs, action, reward, next_obs)
                            self.logger.log(logs, t)
                            obs = next_obs
                    except EpisodeEndException:
                        agent.notify_episode_end()
            except ConnectionResetError:
                self.logger.info(f"Connection with {addr} closed")
                return
