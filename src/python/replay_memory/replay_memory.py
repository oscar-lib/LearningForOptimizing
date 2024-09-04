from collections import deque
from dataclasses import dataclass
from typing import Deque
import torch
from torch_geometric.data import Data
from torch_geometric.loader import DataLoader
from datetime import datetime

import numpy as np
import pickle
import os
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
            obs=self.obs.to(device.index, non_blocking=True),
            available_actions=self.available_actions.to(device),
            actions=self.actions.to(device, non_blocking=True),
            rewards=self.rewards.to(device, non_blocking=True),
            dones=self.dones.to(device, non_blocking=True),
            next_obs=self.next_obs.to(device.index, non_blocking=True),
            next_available_actions=self.next_available_actions.to(device),
        )


@dataclass
class ReplayMemory:
    max_size: int

    def __init__(self, max_size: int):
        self._actions: Deque[int] = deque(maxlen=max_size)
        self._rewards: Deque[float] = deque(maxlen=max_size)
        self._obs: Deque[Observation] = deque(maxlen=max_size)
        self._next_obs: Deque[Observation] = deque(maxlen=max_size)
        self._dones: Deque[bool] = deque(maxlen=max_size)
        self.max_size = max_size
        self.index_episode_start = 0

    def add(self, obs: Observation, action: int, reward: float, next_obs: Observation):
        """Add an item (transition, episode, ...) to the memory"""
        if len(self) == self.max_size and self.index_episode_start > 0:
            self.index_episode_start -= 1
        self._obs.append(obs)
        self._next_obs.append(next_obs)
        self._actions.append(action)
        self._rewards.append(reward)
        self._dones.append(False)

    def end_episode(self):
        self._dones[-1] = True
        return
        # [:-3] to get milliseconds
        timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S-%f")[:-3]
        os.makedirs(f"experiences/{timestamp}/", exist_ok=True)

        actions = []
        dones = []
        rewards = []
        obs = []
        available_actions = []
        next_available_actions = []
        next_obs = []

        for i in range(self.index_episode_start, len(self)):
            actions.append(self._actions[i])
            rewards.append(self._rewards[i])
            dones.append(self._dones[i])
            available_actions.append(self._obs[i].available_actions)
            obs.append(self._obs[i].graph)
            next_available_actions.append(self._next_obs[i].available_actions)
            next_obs.append(self._next_obs[i].graph)

        np.save(f"experiences/{timestamp}/actions.npy", np.array(actions))
        np.save(f"experiences/{timestamp}/rewards.npy", np.array(rewards))
        np.save(f"experiences/{timestamp}/dones.npy", np.array(dones))
        np.save(f"experiences/{timestamp}/available_actions.npy", np.array(available_actions))
        np.save(f"experiences/{timestamp}/next_available_actions.npy", np.array(next_available_actions))
        with open(f"experiences/{timestamp}/obs.pkl", "wb") as f:
            pickle.dump(obs, f)
        with open(f"experiences/{timestamp}/next_obs.pkl", "wb") as f:
            pickle.dump(next_obs, f)
        self.index_episode_start = len(self)

    def sample(self, batch_size: int) -> Batch:
        """Sample the memory to retrieve a `Batch`"""
        indices = np.random.randint(0, len(self), batch_size)
        obs = DataLoader([self._obs[i].graph for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        next_obs = DataLoader([self._next_obs[i].graph for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        actions = torch.tensor([self._actions[i] for i in indices], dtype=torch.long).unsqueeze(-1)
        rewards = torch.tensor([self._rewards[i] for i in indices], dtype=torch.float32)
        dones = torch.tensor([self._dones[i] for i in indices], dtype=torch.float32)
        available_actions = torch.tensor(np.array([self._obs[i].available_actions for i in indices]), dtype=torch.bool)
        next_available_actions = torch.tensor(np.array([self._next_obs[i].available_actions for i in indices]), dtype=torch.bool)
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
        self._obs.clear()
        self._next_obs.clear()
        self._actions.clear()
        self._rewards.clear()
        self.index_episode_start = 0

    @property
    def is_full(self):
        return len(self) == self.max_size

    def __len__(self) -> int:
        return len(self._dones)
