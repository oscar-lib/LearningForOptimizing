from abc import ABC, abstractmethod
from collections import deque
from typing import Optional, Sequence
from torch_geometric.data import Data
import numpy as np
import torch
from optimenv import Observation
from functools import cached_property


class ReplayMemory[T: torch.Tensor | Data](ABC):
    max_size: int

    def __init__(self, max_size: Optional[int]):
        self._actions = deque[int](maxlen=max_size)
        self._rewards = deque[float](maxlen=max_size)
        self._obs = deque[Observation[T]](maxlen=max_size)
        self._next_obs = deque[Observation[T]](maxlen=max_size)
        self._dones = deque[bool](maxlen=max_size)
        self._next_values = deque[float](maxlen=max_size)

    def add(self, obs: Observation[T], action: int, reward: float, next_obs: Observation[T], next_value: float):
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
    def _get_batch(self, indices: Sequence[int]) -> "Batch[T]":
        """Retrieve a `Batch` from the memory given a list of indices"""

    def sample(self, batch_size: int) -> "Batch[T]":
        """Randomly sample the memory to retrieve a `Batch`"""
        indices = np.random.randint(0, len(self), batch_size)
        return self._get_batch(indices.tolist())

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


class Batch[T: torch.Tensor | Data]:
    def __init__(self, memory: ReplayMemory[T], indices: Sequence[int], device: torch.device):
        self.memory = memory
        self.indices = indices
        self.device = device
        self.size = len(indices)

    def to(self, device: torch.device):
        """Send the tensors to the given device"""
        self.device = device
        for key, value in self.__dict__.items():
            if isinstance(value, torch.Tensor):
                value = value.to(device, non_blocking=True)
                setattr(self, key, value)
        return self

    def __len__(self):
        return self.size

    @cached_property
    @abstractmethod
    def obs(self) -> T: ...

    @cached_property
    @abstractmethod
    def next_obs(self) -> T: ...

    @cached_property
    def available_actions(self):
        return torch.stack([self.memory._obs[i].available_actions for i in self.indices]).to(self.device)

    @cached_property
    def actions(self):
        return torch.tensor([self.memory._actions[i] for i in self.indices], dtype=torch.long).unsqueeze(-1).to(self.device)

    @cached_property
    def rewards(self):
        return torch.tensor([self.memory._rewards[i] for i in self.indices], dtype=torch.float32).to(self.device)

    @cached_property
    def dones(self):
        return torch.tensor([self.memory._dones[i] for i in self.indices], dtype=torch.bool).to(self.device)

    @cached_property
    def next_available_actions(self):
        return torch.stack([self.memory._next_obs[i].available_actions for i in self.indices]).to(self.device)

    @cached_property
    def next_values(self):
        return torch.tensor([self.memory._next_values[i] for i in self.indices], dtype=torch.float32).to(self.device)
