from abc import ABC, abstractmethod
from collections import deque
from typing import Optional
from torch_geometric.data import Data
import numpy as np
import torch
from optimenv import Observation


class Batch[T: torch.Tensor | Data]:
    def __init__(
        self,
        obs: T,
        available_actions: torch.Tensor,
        actions: torch.Tensor,
        rewards: torch.Tensor,
        dones: torch.Tensor,
        next_obs: T,
        next_available_actions: torch.Tensor,
        next_values: torch.Tensor,
    ):
        self.obs = obs
        self.available_actions = available_actions
        self.actions = actions
        self.rewards = rewards
        self.dones = dones
        self.next_obs = next_obs
        self.next_available_actions = next_available_actions
        self.next_values = next_values
        self.size = len(rewards)

    def to(self, device: torch.device) -> "Batch":
        return Batch(
            obs=self.obs.to(device.index, non_blocking=True),
            available_actions=self.available_actions.to(device),
            actions=self.actions.to(device, non_blocking=True),
            rewards=self.rewards.to(device, non_blocking=True),
            dones=self.dones.to(device, non_blocking=True),
            next_obs=self.next_obs.to(device.index, non_blocking=True),
            next_available_actions=self.next_available_actions.to(device, non_blocking=True),
            next_values=self.next_values.to(device, non_blocking=True),
        )

    def __len__(self):
        return self.size


class ReplayMemory[T: torch.Tensor](ABC):
    max_size: int

    def __init__(self, max_size: Optional[int]):
        self._actions = deque[int](maxlen=max_size)
        self._rewards = deque[float](maxlen=max_size)
        self._obs = deque[Observation](maxlen=max_size)
        self._next_obs = deque[Observation](maxlen=max_size)
        self._dones = deque[bool](maxlen=max_size)
        self._next_values = deque[float](maxlen=max_size)

    def add(self, obs: Observation, action: int, reward: float, next_obs: Observation, next_value: float):
        """Add an item (transition, episode, ...) to the memory"""
        self._obs.append(obs)
        self._next_obs.append(next_obs)
        self._actions.append(action)
        self._rewards.append(reward)
        self._dones.append(False)
        self._next_values.append(next_value)

    def end_episode(self):
        self._dones[-1] = True

    @abstractmethod
    def _get_batch(self, indices: np.ndarray) -> Batch[T]:
        """Retrieve a `Batch` from the memory given a list of indices"""

    def sample(self, batch_size: int) -> Batch[T]:
        """Randomly sample the memory to retrieve a `Batch`"""
        indices = np.random.randint(0, len(self), batch_size)
        return self._get_batch(indices)

    def get_all(self):
        """Return all items in the memory as a `Batch`"""
        indices = np.arange(len(self))
        return self._get_batch(indices)

    def can_sample(self, batch_size: int) -> bool:
        """Return whether the memory contains enough items to sample a batch of the given size"""
        return len(self) >= batch_size

    def clear(self):
        self._dones.clear()
        self._obs.clear()
        self._next_obs.clear()
        self._actions.clear()
        self._rewards.clear()
        self._next_values.clear()

    @property
    def is_full(self):
        return len(self) == self.max_size

    def __len__(self) -> int:
        return len(self._dones)
