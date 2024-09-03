import torch
from dqn import DQN
from logger import Logger
from bridge.protocol.message import Message, MessageType
from bridge import Bridge
from optimenv import EpisodeEndException, OptimEnv
from problem import Problem


class Runner:
    def __init__(self, bridge: Bridge):
        self.bridge = bridge

    def run(self, device: torch.device):
        logger = Logger(wandb=True, csv=True)
        logger.info("Starting runner")
        try:
            req = self.bridge.recv()
            if req.type != MessageType.STATIC_DATA:
                error = f"Expected message of type {MessageType.STATIC_DATA} from the client, got {req.type}"
                logger.error(error)
                self.bridge.send(Message.error(error).to_bytes())
                return
            problem = Problem.parse(req.body)
            self.bridge.send(Message.ack().to_bytes())
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
