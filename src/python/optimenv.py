import json
import logging
import struct
from dataclasses import dataclass
from socket import socket

from bridge.protocol.message import Message, MessageType
from problem import Problem
from torch_geometric.data import Data


class EpisodeEndException(Exception):
    pass


@dataclass
class Observation:
    graph: Data
    available_actions: list[bool]


class OptimEnv:
    def __init__(self, problem: Problem, sock: socket):
        self.problem = problem
        self.socket = sock
        self.pending_msg = None

    def reset(self) -> Observation:
        return self.observation()

    def step(self, action: int) -> tuple[Observation, float]:
        self.socket.send(Message.inference_resp(action).to_bytes())
        req = Message.recv(self.socket)
        if req.type == MessageType.END_EPISODE:
            raise EpisodeEndException()
        if req.type != MessageType.REWARD:
            raise ValueError(f"Expected message of type {MessageType.REWARD.name} from the client, got {req.type.name}")
        reward = struct.unpack(">f", req.body)[0]
        obs_ = self.observation()
        return obs_, reward

    def observation(self):
        req = Message.recv(self.socket)
        if req.type == MessageType.END_EPISODE:
            raise EpisodeEndException()
        if req.type != MessageType.ACTION_REQ:
            raise ValueError(f"Expected message of type {MessageType.ACTION_REQ.name} from the client, got {req.type.name}")
        data = json.loads(req.body)
        routes = data["routes"]
        available_actions = data["available"]
        data = self.problem.build_agent_input(routes)
        # TODO: move build_agent_input to the environment
        return Observation(
            data,
            available_actions,
        )
