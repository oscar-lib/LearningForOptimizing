import os
import pickle
from abc import ABC, abstractmethod
from collections import deque
from datetime import datetime
from typing import Deque

import numpy as np
import torch
from optimenv import Observation


class Batch[T: torch.Tensor]:
    def __init__(
        self,
        obs: T,
        available_actions: torch.Tensor,
        actions: torch.Tensor,
        rewards: torch.Tensor,
        dones: torch.Tensor,
        next_obs: T,
        next_available_actions: torch.Tensor,
    ):
        self.obs = obs
        self.available_actions = available_actions
        self.actions = actions
        self.rewards = rewards
        self.dones = dones
        self.next_obs = next_obs
        self.next_available_actions = next_available_actions
        self.size = len(rewards)

    def to(self, device: torch.device) -> "Batch":
        return Batch(
            obs=self.obs.to(device, non_blocking=True),
            available_actions=self.available_actions.to(device),
            actions=self.actions.to(device, non_blocking=True),
            rewards=self.rewards.to(device, non_blocking=True),
            dones=self.dones.to(device, non_blocking=True),
            next_obs=self.next_obs.to(device, non_blocking=True),
            next_available_actions=self.next_available_actions.to(device, non_blocking=True),
        )

    def __len__(self):
        return self.size


class ReplayMemory[T: torch.Tensor](ABC):
    max_size: int

    def __init__(self, max_size: int):
        self._actions: Deque[int] = deque(maxlen=max_size)
        self._rewards: Deque[float] = deque(maxlen=max_size)
        self._obs: Deque[Observation] = deque(maxlen=max_size)
        self._next_obs: Deque[Observation] = deque(maxlen=max_size)
        self._dones: Deque[bool] = deque(maxlen=max_size)
        self.max_size = max_size
        self.index_episode_start = 0
        self.perform_check = False

    def add(self, obs: Observation, action: int, reward: float, next_obs: Observation):
        """Add an item (transition, episode, ...) to the memory"""
        if len(self) == self.max_size and self.index_episode_start > 0:
            self.index_episode_start -= 1
        if self.perform_check:
            assert torch.equal(obs.data, self._next_obs[-1].data)
        self.perform_check = True
        self._obs.append(obs)
        self._next_obs.append(next_obs)
        self._actions.append(action)
        self._rewards.append(reward)
        self._dones.append(False)

    def end_episode(self):
        self._dones[-1] = True
        self.perform_check = False
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
            obs.append(self._obs[i].data)
            next_available_actions.append(self._next_obs[i].available_actions)
            next_obs.append(self._next_obs[i].data)

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
        self.index_episode_start = 0

    @property
    def is_full(self):
        return len(self) == self.max_size

    def __len__(self) -> int:
        return len(self._dones)
