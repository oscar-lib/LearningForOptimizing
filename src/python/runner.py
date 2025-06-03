from dataclasses import dataclass
from typing import Literal, Optional
import torch
from algos import DQN, PPO, Algo
from logger import Logger
from bridge.protocol.message import Message, MessageType
from bridge import Bridge
from optimenv import EpisodeEndException, OptimEnv
from problem import PDPTW, CSP
from replay_memory import ReplayMemory


@dataclass
class Params:
    lr: float
    ddqn: bool
    batch_size: int
    clipping: Optional[float]
    epsilon: float


class Runner:
    def __init__(self, bridge: Bridge):
        self.bridge = bridge

    def run(self, device: torch.device, algo: Literal["dqn", "ppo"], args: Params):
        logger = Logger(csv=True)
        logger.info("Starting runner")
        try:
            problem = self._retrieve_problem_data(logger)
            agent = self._create_agent(problem, algo, args).to(device)
            env = OptimEnv(problem, self.bridge)
            t = 0
            while True:
                obs = env.reset()
                try:
                    while True:
                        t += 1
                        action, action_data = agent.select_action(obs)
                        next_obs, reward = env.step(action)
                        logs = agent.learn(t, obs, action, reward, next_obs)
                        logs = logs | {"action": action, "reward": reward} | {f"action-{i}": x for i, x in enumerate(action_data)}
                        logger.log(logs, t)
                        obs = next_obs
                except EpisodeEndException:
                    agent.notify_episode_end()
        except ConnectionResetError:
            logger.error("Connection with remote closed")
            return
        except KeyboardInterrupt:
            pass
        logger.info("Stopping runner")

    def _retrieve_problem_data(self, logger: Logger):
        req = self.bridge.recv()
        match req.type:
            case MessageType.STATIC_DATA_PDPTW:
                problem = PDPTW.parse(req.body)
                self.bridge.send(Message.ack().to_bytes())
                return problem
            case MessageType.STATIC_DATA_CSP:
                problem = CSP.parse(req.body)
                self.bridge.send(Message.ack().to_bytes())
                return problem
            case other:
                error = f"Expected message of type {MessageType.STATIC_DATA_PDPTW} from the client, got {other}"
                logger.error(error)
                self.bridge.send(Message.error(error).to_bytes())
                raise Exception(error)

    def _create_agent(self, problem: PDPTW | CSP, algo: Literal["dqn", "ppo"], args: Params) -> Algo:
        match algo:
            case "dqn":
                match problem:
                    case PDPTW():
                        from nn import QNetGNN

                        qnetwork = QNetGNN(problem)
                    case CSP():
                        from nn import CNN

                        qnetwork = CNN(problem)
                return DQN(
                    qnetwork=qnetwork,
                    memory=ReplayMemory(1000),
                    double_qlearning=args.ddqn,
                    grad_norm_clipping=args.clipping,
                    lr=args.lr,
                    epsilon=args.epsilon,
                    batch_size=args.batch_size,
                )
            case "ppo":
                assert isinstance(problem, PDPTW)
                return PPO.default(problem)
        raise Exception(f"Unknown algorithm: {algo}")
