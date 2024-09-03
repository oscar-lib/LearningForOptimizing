import json
import struct
from dataclasses import dataclass

from bridge import Bridge
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
    def __init__(self, problem: Problem, bridge: Bridge):
        self.problem = problem
        self.bridge = bridge
        self.pending_msg = None

    def reset(self) -> Data:
        return self.observation()

    def step(self, action: int) -> tuple[Data, float]:
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
        routes = json.loads(req.body)
        data = self.problem.build_agent_input(routes)
        # TODO: move build_agent_input to the environment
        return data
