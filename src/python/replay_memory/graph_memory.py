from .replay_memory import ReplayMemory, Batch

import torch
from torch_geometric.loader import DataLoader

import numpy as np


class GraphReplayMemory(ReplayMemory):
    def __init__(self, max_size: int):
        super().__init__(max_size)

    def _get_batch(self, indices: np.ndarray):
        batch_size = len(indices)
        obs = DataLoader([self._obs[i].data for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        next_obs = DataLoader([self._next_obs[i].data for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
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
