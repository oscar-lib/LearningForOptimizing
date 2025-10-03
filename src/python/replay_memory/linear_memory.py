from numpy import ndarray
from .replay_memory import ReplayMemory, Batch
import torch


class LinearMemory(ReplayMemory[torch.Tensor]):
    def _get_batch(self, indices: ndarray) -> Batch[torch.Tensor]:
        return Batch(
            obs=torch.stack([self._obs[i].data for i in indices]),
            available_actions=torch.stack([self._obs[i].available_actions for i in indices]),
            actions=torch.tensor([self._actions[i] for i in indices], dtype=torch.long).unsqueeze(-1),
            rewards=torch.tensor([self._rewards[i] for i in indices], dtype=torch.float32),
            dones=torch.tensor([self._dones[i] for i in indices], dtype=torch.bool),
            next_obs=torch.stack([self._next_obs[i].data for i in indices]),
            next_available_actions=torch.stack([self._next_obs[i].available_actions for i in indices]),
            next_values=torch.tensor([self._next_values[i] for i in indices], dtype=torch.float32),
        )
