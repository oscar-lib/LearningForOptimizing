from .replay_memory import ReplayMemory, Batch
import torch
from typing import Sequence
from functools import cached_property


class LinearMemory(ReplayMemory[torch.Tensor]):
    def _get_batch(self, indices: Sequence[int], device: torch.device) -> Batch[torch.Tensor]:
        return LinearBatch(self, indices, device)


class LinearBatch(Batch[torch.Tensor]):
    @cached_property
    def obs(self):
        return torch.stack([self.memory._obs[i].data for i in self.indices]).to(self.device)

    @cached_property
    def next_obs(self):
        return torch.stack([self.memory._next_obs[i].data for i in self.indices]).to(self.device)
