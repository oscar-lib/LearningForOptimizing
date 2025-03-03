from dataclasses import dataclass
import json
import torch
from .problem import Problem


@dataclass
class Machine:
    id: int
    max_seq: int
    """The maximal number of time steps that can be filled in any window of size `self.seq_len`"""
    seq_len: int
    """The length of the sequence concerned for `self.max_seq`."""

    @property
    def max_utilization(self):
        return self.max_seq / self.seq_len


@dataclass
class CarConfig:
    id: int
    amount_to_make: int
    parts: list[Machine]


@dataclass
class CSP(Problem[torch.Tensor]):
    machines: list[Machine]
    cars: list[CarConfig]

    @staticmethod
    def parse(bdata: bytes) -> "CSP":
        data = json.loads(bdata)
        max_sequences = data["maxCarsWithOptInSeq"]
        seq_lengths = data["optSeqLen"]

        machines = list[Machine]()
        for i, (max_seq, seq_len) in enumerate(zip(max_sequences, seq_lengths)):
            machines.append(Machine(i, max_seq, seq_len))

        recipes = data["configs"]
        cars = list[CarConfig]()
        for recipe in recipes:
            cars.append(CarConfig(recipe["id"], recipe["nCarsWithConf"], recipe["optInConf"]))
        res = CSP(machines, cars)
        print(res)
        return res

    def build_agent_input(self, data: dict) -> torch.Tensor:
        raise NotImplementedError()
