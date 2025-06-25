from dataclasses import dataclass
from typing import Self
import orjson
import torch
from .problem import Problem


@dataclass
class Option:
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
    n_to_make: int
    options: torch.Tensor

    def __init__(self, id: int, n_to_make: int, options: list[bool]):
        self.id = id
        self.options = torch.tensor(options, dtype=torch.float32)
        self.n_to_make = n_to_make

    def has_option(self, option_id: int):
        return self.options[option_id] == 1

    def __eq__(self, other) -> bool:
        if not isinstance(other, CarConfig):
            return False
        if self.id != other.id:
            return False
        return self.n_to_make == other.n_to_make


@dataclass
class CSP(Problem[torch.Tensor]):
    options: list[Option]
    cars: list[CarConfig]
    n_cars: int
    """
    The total number of cars to make, which is the sum of `n_to_make` for all car configurations.
    """
    n_actions: int
    options_data: torch.Tensor
    """
    A tensor of shape (n_options, 2) where each row contains the max_seq and seq_len for each option.
    """

    def __init__(self, n_actions: int, options: list[Option], cars: list[CarConfig]):
        super().__init__()
        self.options = options
        self.cars = cars
        self.n_actions = n_actions
        self._cars_data = torch.stack([car.options for car in cars])
        """
        A tensor of shape (n_cars, n_options) where each row corresponds to the options that a car has.
        """
        self.options_data = torch.tensor([[option.max_seq, option.seq_len] for option in options], dtype=torch.float32)
        self.n_cars = sum(car.n_to_make for car in cars)

    @staticmethod
    def parse(bdata: bytes) -> "CSP":
        data = orjson.loads(bdata)
        problem = data["problem"]["instance"]
        n_actions = data["nActions"]
        max_sequences = problem["maxCarsWithOptInSeq"]
        seq_lengths = problem["optSeqLen"]

        options = list[Option]()
        for i, (max_seq, seq_len) in enumerate(zip(max_sequences, seq_lengths)):
            options.append(Option(i, max_seq, seq_len))

        recipes = problem["configs"]
        cars = list[CarConfig]()
        for recipe in recipes:
            cars.append(CarConfig(recipe["id"], recipe["nCarsWithConf"], recipe["optInConf"]))
        res = CSP(n_actions, options, cars)
        return res

    def build_agent_input(self, data: dict, device: torch.device) -> torch.Tensor:
        """
        The current state of the problem is represented by the sequence of options to make.
        """
        sequence = data["state"]
        busy_options = torch.zeros(self.n_options, self.n_cars, dtype=torch.float32)
        for i, car_num in enumerate(sequence):
            busy_options[:, i] = self._cars_data[car_num]
        return busy_options.to(device)

    @property
    def n_car_configs(self) -> int:
        return len(self.cars)

    @property
    def n_options(self) -> int:
        return len(self.options)

    @property
    def instance_shape(self):
        return (self.n_cars, self.n_options), (self.n_options, 2)

    @property
    def state_shape(self):
        return (1, self.n_options, self.n_cars)

    def is_compatible_with(self, other) -> bool:
        """
        Check if this CSP is compatible with another CSP.
        Two CSPs are compatible if they have the same number of actions and options.
        """
        if not isinstance(other, CSP):
            return False
        return self.n_actions == other.n_actions and self.n_options == other.n_options

    @property
    def max_seq_length(self) -> int:
        return max(option.seq_len for option in self.options)

    def __eq__(self, other) -> bool:
        if not isinstance(other, CSP):
            return False
        if self.n_actions != other.n_actions:
            return False
        if self.options != other.options:
            return False
        if self.cars != other.cars:
            return False
        return True
