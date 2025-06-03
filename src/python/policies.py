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
    def get_action(self, qvalues: np.ndarray, available_actions: np.ndarray) -> np.ndarray:
        """
        Choose an action based on the given qvalues and avalable actions.
        Returns the chosen action.
        """

    @abstractmethod
    def update(self, time_step: int) -> dict[str, float]:
        """Update the object and return the corresponding logs."""


@dataclass
class SoftmaxPolicy(Policy):
    """Softmax policy"""

    tau: float

    def __init__(self, n_actions: int, tau: float = 1.0):
        super().__init__()
        self.actions = np.arange(n_actions, dtype=np.int64)
        self.tau = tau

    def get_action(self, qvalues: npt.NDArray[np.float32], available_actions: npt.NDArray[np.float32]) -> npt.NDArray[np.int64]:
        qvalues[available_actions == 0.0] = -np.inf
        exp = np.exp(qvalues / self.tau)
        probs = exp / np.sum(exp, axis=-1, keepdims=True)
        chosen_actions = [np.random.choice(self.actions, p=agent_probs) for agent_probs in probs]
        return np.array(chosen_actions)

    def update(self, _: int) -> dict[str, float]:
        return {"softmax-tau": self.tau}


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

    def update(self, step_num: int):
        self.epsilon.update(step_num)
        return {"epsilon": self.epsilon.value}


@dataclass
class ArgMax(Policy):
    """Exploiting the strategy"""

    def __init__(self):
        super().__init__()

    def get_action(self, qvalues: np.ndarray, available_actions: npt.NDArray[np.float32]) -> np.ndarray:
        qvalues[available_actions == 0.0] = -np.inf
        actions = qvalues.argmax(-1)
        return actions

    def update(self, step_num: int):
        return {}
