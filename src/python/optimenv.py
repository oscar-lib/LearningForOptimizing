import struct
from dataclasses import dataclass
from typing import Any

import orjson
import torch
from bridge import Bridge
from bridge.protocol.message import Message, MessageType
from problem import Problem


class EpisodeEndException(Exception):
    pass


class RegisterTransition(Exception):
    def __init__(self, json_data: dict[str, Any]) -> None:
        super().__init__()
        self.data = json_data


@dataclass
class Observation[T]:
    data: T
    available_actions: torch.Tensor


class OptimEnv[T]:
    def __init__(self, problem: Problem[T], bridge: Bridge, device: torch.device):
        self.problem = problem
        self.bridge = bridge
        self.device = device

    def reset(self):
        return self.observation()

    def step(self, action: int):
        self.bridge.send(Message.inference_resp(action).to_bytes())
        req = self.bridge.recv()
        if req.type == MessageType.END_EPISODE:
            raise EpisodeEndException()
        if req.type == MessageType.TRANSITION:
            raise RegisterTransition(orjson.loads(req.body))
        if req.type != MessageType.REWARD:
            raise ValueError(f"Expected message of type {MessageType.REWARD.name} from the client, got {req.type.name}")
        reward = struct.unpack(">f", req.body[:4])[0]
        obj = struct.unpack(">f", req.body[4:8])[0]
        obs_ = self.observation()
        return obs_, reward, obj

    def observation(self):
        req = self.bridge.recv()
        if req.type == MessageType.END_EPISODE:
            raise EpisodeEndException()
        if req.type == MessageType.TRANSITION:
            raise RegisterTransition(orjson.loads(req.body))
        if req.type != MessageType.ACTION_REQ:
            raise ValueError(f"Expected message of type {MessageType.ACTION_REQ.name} from the client, got {req.type.name}")
        data = orjson.loads(req.body)
        available_actions = data["available"]
        data = self.problem.build_agent_input(data, self.device)
        return Observation(data=data, available_actions=torch.tensor(available_actions, dtype=torch.bool, device=self.device))
