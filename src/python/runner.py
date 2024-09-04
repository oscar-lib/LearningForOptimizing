from typing import Literal
import torch
from algos.dqn import DQN
from logger import Logger
from bridge.protocol.message import Message, MessageType
from bridge import Bridge
from optimenv import EpisodeEndException, OptimEnv
from problem import Problem


class Runner:
    def __init__(self, bridge: Bridge):
        self.bridge = bridge

    def run(self, device: torch.device, algo: Literal["dqn", "ppo"]):
        logger = Logger(wandb=False, csv=True)
        logger.info("Starting runner")
        try:
            problem = self._retrieve_problem_data(logger)
            agent = DQN.default(problem).to(device)
            env = OptimEnv(problem, self.bridge)
            t = 0
            while True:
                obs = env.reset()
                try:
                    while True:
                        t += 1
                        action, qvalues = agent.select_action(obs)
                        next_obs, reward = env.step(action)
                        logs = agent.learn(t, obs, action, reward, next_obs)
                        logs = logs | {"action": action, "reward": reward} | {f"q-{i}": q for i, q in enumerate(qvalues)}
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
        print("End of the runner")

    def _retrieve_problem_data(self, logger: Logger):
        req = self.bridge.recv()
        if req.type != MessageType.STATIC_DATA:
            error = f"Expected message of type {MessageType.STATIC_DATA} from the client, got {req.type}"
            logger.error(error)
            self.bridge.send(Message.error(error).to_bytes())
            raise Exception(error)
        problem = Problem.parse(req.body)
        self.bridge.send(Message.ack().to_bytes())
        return problem

    def _create_agent(self, problem: Problem, algo: Literal["dqn", "ppo"], device: torch.device):
        if algo == "dqn":
            return DQN.default(problem).to(device)
        raise Exception(f"Unknown algorithm: {algo}")
