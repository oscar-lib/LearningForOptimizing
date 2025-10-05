from functools import cached_property
from typing import Sequence

import torch
from torch_geometric.data import Batch as GeoBatch
from torch_geometric.data import Data

from .replay_memory import Batch, ReplayMemory


class GraphReplayMemory(ReplayMemory):
    def __init__(self, max_size: int):
        super().__init__(max_size)

    def _get_batch(self, indices: Sequence[int], device: torch.device) -> Batch[Data]:
        return GraphBatch(self, indices, device)


class GraphBatch(Batch[Data]):
    @cached_property
    def obs(self):
        # DataLoader([self._obs[i].data for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        # TODO: check how to move to device efficiently
        return GeoBatch.from_data_list([self.memory._obs[i].data for i in self.indices])

    @cached_property
    def next_obs(self):
        # DataLoader([self._next_obs[i].data for i in indices], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        return GeoBatch.from_data_list([self.memory._next_obs[i].data for i in self.indices])
