from abc import abstractmethod
import random
import numpy as np
import numpy.typing as npt
from dataclasses import dataclass

from utils import schedule


class Policy:
    """
    A policy takes decides which action to take given an input.
    """

    name: str

    def __init__(self):
        self.name = self.__class__.__name__

    @abstractmethod
    def get_action(self, qvalues: np.ndarray, available_actions: np.ndarray, /) -> int:
        """
        Choose an action based on the given qvalues and avalable actions.
        Returns the chosen action.
        """

    @abstractmethod
    def update(self, time_step: int, /) -> dict[str, float]:
        """Update the object and return the corresponding logs."""


@dataclass
class EpsilonGreedy(Policy):
    """Epsilon Greedy policy"""

    epsilon: schedule.Schedule

    def __init__(self, epsilon: schedule.Schedule):
        super().__init__()
        self.epsilon = epsilon

    @classmethod
    def linear(cls, start_eps: float, min_eps: float, n_steps: int):
        return cls(schedule.LinearSchedule(start_eps, min_eps, n_steps))

    @classmethod
    def exponential(cls, start_eps: float, min_eps: float, n_steps: int):
        return cls(schedule.ExpSchedule(start_eps, min_eps, n_steps))

    @classmethod
    def constant(cls, eps: float):
        return cls(schedule.ConstantSchedule(eps))

    def get_action(self, qvalues: np.ndarray, available_actions: np.ndarray) -> int:
        r = random.random()
        if self.epsilon.value < r:
            return int(random.choice(np.nonzero(available_actions)[0]))
        qvalues[available_actions == 0] = -np.inf
        return int(np.argmax(qvalues).item())

    def update(self, time_step: int):
        self.epsilon.update(time_step)
        return {"epsilon": self.epsilon.value}


@dataclass
class ArgMax(Policy):
    """Exploiting the strategy"""

    def __init__(self):
        super().__init__()

    def get_action(self, qvalues: np.ndarray, available_actions: npt.NDArray[np.float32]):
        qvalues[available_actions == 0.0] = -np.inf
        actions = qvalues.argmax(-1)
        return actions.item()

    def update(self, time_step: int):
        return {}
