from dataclasses import dataclass
import json
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
    amount_to_make: int
    options: list[Option]


@dataclass
class CSP(Problem[torch.Tensor]):
    options: list[Option]
    cars: list[CarConfig]
    n_actions: int
    cars_data: torch.Tensor
    """
    A tensor of shape (n_cars, n_options) where each row corresponds to the options that a car has.
    """
    options_data: torch.Tensor
    """
    A tensor of shape (n_options, 2) where each row contains the max_seq and seq_len for each option.
    """

    def __init__(self, n_actions: int, options: list[Option], cars: list[CarConfig]):
        super().__init__()
        self.machines = options
        self.cars = cars
        self.n_actions = n_actions
        cars_data = []
        for car in cars:
            car_data = [0] * len(options)
            for i, option in enumerate(car.options):
                if option:
                    car_data[i] = 1
            cars_data += [car_data] * car.amount_to_make  # One line per car to make
        options_data = [[option.max_seq, option.seq_len] for option in options]
        self.cars_data = torch.tensor(cars_data, dtype=torch.float32)
        self.options_data = torch.tensor(options_data, dtype=torch.float32)

    @staticmethod
    def parse(bdata: bytes) -> "CSP":
        data = json.loads(bdata)
        max_sequences = data["maxCarsWithOptInSeq"]
        seq_lengths = data["optSeqLen"]

        options = list[Option]()
        for i, (max_seq, seq_len) in enumerate(zip(max_sequences, seq_lengths)):
            options.append(Option(i, max_seq, seq_len))

        recipes = data["configs"]
        cars = list[CarConfig]()
        for recipe in recipes:
            required_options = [options[i] for i, has_option in enumerate(recipe["optInConf"]) if has_option]
            cars.append(CarConfig(recipe["id"], recipe["nCarsWithConf"], required_options))
        res = CSP(data["nActions"], options, cars)
        print(res)
        return res

    def build_agent_input(self, data: dict) -> torch.Tensor:
        raise NotImplementedError()

    @property
    def n_cars(self) -> int:
        return len(self.cars)

    @property
    def n_options(self) -> int:
        return len(self.machines)

    @property
    def instance_shape(self):
        return (self.n_cars, self.n_options), (self.n_options, 2)

    @property
    def state_shape(self):
        return (1, self.n_options, self.n_cars)
