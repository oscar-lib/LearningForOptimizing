import json
import struct
from dataclasses import dataclass

import torch

from bridge import Bridge
from bridge.protocol.message import Message, MessageType
from problem import Problem


class EpisodeEndException(Exception):
    pass


@dataclass
class Observation[T]:
    graph: T
    available_actions: torch.Tensor


class OptimEnv[T]:
    def __init__(self, problem: Problem[T], bridge: Bridge):
        self.problem = problem
        self.bridge = bridge
        self.pending_msg = None

    def reset(self):
        return self.observation()

    def step(self, action: int) -> tuple[Observation, float]:
        self.bridge.send(Message.inference_resp(action).to_bytes())
        req = self.bridge.recv()
        if req.type == MessageType.END_EPISODE:
            raise EpisodeEndException()
        if req.type != MessageType.REWARD:
            raise ValueError(f"Expected message of type {MessageType.REWARD.name} from the client, got {req.type.name}")
        reward = struct.unpack(">f", req.body)[0]
        obs_ = self.observation()
        return obs_, reward

    def observation(self):
        req = self.bridge.recv()
        if req.type == MessageType.END_EPISODE:
            raise EpisodeEndException()
        if req.type != MessageType.ACTION_REQ:
            raise ValueError(f"Expected message of type {MessageType.ACTION_REQ.name} from the client, got {req.type.name}")
        data = json.loads(req.body)
        available_actions = data["available"]
        data = self.problem.build_agent_input(data)
        return Observation(graph=data, available_actions=torch.tensor(available_actions, dtype=torch.bool))
