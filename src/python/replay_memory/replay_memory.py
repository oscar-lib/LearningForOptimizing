from collections import deque
from dataclasses import dataclass
from typing import Deque
import torch
from torch_geometric.data import Data
from torch_geometric.loader import DataLoader

import numpy as np

from optimenv import Observation


@dataclass
class Batch:
    obs: Data
    available_actions: torch.Tensor
    actions: torch.Tensor
    rewards: torch.Tensor
    dones: torch.Tensor
    next_obs: Data
    next_available_actions: torch.Tensor

    def to(self, device: torch.device) -> "Batch":
        return Batch(
            obs=self.obs.to(device.index),
            available_actions=self.available_actions.to(device),
            actions=self.actions.to(device),
            rewards=self.rewards.to(device),
            dones=self.dones.to(device),
            next_obs=self.next_obs.to(device.index),
            next_available_actions=self.next_available_actions.to(device),
        )


@dataclass
class ReplayMemory:
    max_size: int
    name: str

    def __init__(self, max_size: int):
        self._actions: Deque[int] = deque(maxlen=max_size)
        self._rewards: Deque[float] = deque(maxlen=max_size)
        self._obs: Deque[Observation] = deque(maxlen=max_size)
        self._next_obs: Deque[Observation] = deque(maxlen=max_size)
        self._dones: Deque[bool] = deque(maxlen=max_size)
        self.max_size = max_size

    def add(self, obs: Observation, action: int, reward: float, next_obs: Observation):
        """Add an item (transition, episode, ...) to the memory"""
        self._obs.append(obs)
        self._next_obs.append(next_obs)
        self._actions.append(action)
        self._rewards.append(reward)
        self._dones.append(False)

    def end_episode(self):
        self._dones[-1] = True

    def sample(self, batch_size: int) -> Batch:
        """Sample the memory to retrieve a `Batch`"""
        indices = np.random.randint(0, len(self), batch_size)
        obs = DataLoader([self._obs[i].graph for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        available_actions = torch.tensor([self._obs[i].available_actions for i in indices], dtype=torch.float32)
        next_obs = DataLoader([self._next_obs[i].graph for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        next_available_actions = torch.tensor([self._next_obs[i].available_actions for i in indices], dtype=torch.float32)
        actions = torch.tensor([self._actions[i] for i in indices], dtype=torch.long).unsqueeze(-1)
        rewards = torch.tensor([self._rewards[i] for i in indices], dtype=torch.float32)
        dones = torch.tensor([self._dones[i] for i in indices], dtype=torch.float32)
        return Batch(
            obs=obs,
            available_actions=available_actions,
            actions=actions,
            rewards=rewards,
            dones=dones,
            next_obs=next_obs,
            next_available_actions=next_available_actions,
        )

    def can_sample(self, batch_size: int) -> bool:
        """Return whether the memory contains enough items to sample a batch of the given size"""
        return len(self) >= batch_size

    def clear(self):
        self._dones.clear()

    @property
    def is_full(self):
        return len(self) == self.max_size

    def __len__(self) -> int:
        return len(self._dones)
