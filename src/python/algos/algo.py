from abc import ABC, abstractmethod
from typing import Self

import numpy as np
import torch
from optimenv import Observation


class Algo(ABC):
    @abstractmethod
    def select_action(self, obs: Observation) -> tuple[int, np.ndarray]:
        """
        Select an action given an observation.

        Returns:
            - the action
            - the qvalues or policy logits according to the algorithm
        """

    @abstractmethod
    def notify_episode_end(self):
        """Called when the episode ends."""

    @abstractmethod
    def learn(self, time_step: int, obs: Observation, action: int, reward: float, next_obs: Observation) -> dict[str, float]:
        """
        Called after each interaction with the environment.

        Returns:
            - a dictionary of logs
        """

    @abstractmethod
    def to(self, device: torch.device) -> Self:
        """
        Move the algorithm to the given device.
        """
