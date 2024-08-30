from copy import deepcopy
from dataclasses import dataclass
from typing import Optional
import numpy as np
from torch_geometric.data import Data

import torch

from optimenv import Observation
from policies import EpsilonGreedy

from problem import Problem
from replay_memory.prioritized_memory import PrioritizedMemory
from qtarget_updater import HardUpdate
from replay_memory.replay_memory import Batch, ReplayMemory


@dataclass
class DQN:
    qnetwork: torch.nn.Module
    memory: ReplayMemory
    gamma: float
    batch_size: int
    grad_norm_clipping: Optional[float]
    lr: float

    def __init__(
        self,
        qnetwork: torch.nn.Module,
        memory: ReplayMemory,
        gamma: float = 0.99,
        batch_size: int = 64,
        lr: float = 1e-4,
        grad_norm_clipping: Optional[float] = None,
        double_qlearning: bool = False,
    ):
        self.qnetwork = qnetwork
        self.qtarget = deepcopy(qnetwork)
        self.memory = memory
        self.gamma = gamma
        self.batch_size = batch_size
        self.double_qlearning = double_qlearning
        self.policy = EpsilonGreedy.constant(0.1)
        self.lr = lr
        self.optimiser = torch.optim.Adam(self.qnetwork.parameters(), lr=lr)  # type: ignore
        # Parameters and optimiser
        self.grad_norm_clipping = grad_norm_clipping
        self.target_updater = HardUpdate(update_period=100)

    @classmethod
    def default(cls, problem: Problem):
        from gnn import QNetGNN

        return DQN(
            qnetwork=QNetGNN(problem),
            memory=ReplayMemory(1000),
            double_qlearning=False,
            grad_norm_clipping=None,
        )

    def select_action(self, obs: Observation) -> int:
        qvalues = self.compute_qvalues(obs.graph).squeeze().numpy(force=True)
        action = self.policy.get_action(qvalues, np.array(obs.available_actions))
        return action

    def compute_qvalues(self, state) -> torch.Tensor:
        return self.qnetwork.forward(state)

    def notify_episode_end(self):
        self.memory.end_episode()

    def learn(self, time_step: int, obs: Observation, action: int, reward: float, next_obs: Observation) -> dict[str, float]:
        self.memory.add(obs, action, reward, next_obs)
        if not self._can_update():
            return {}
        logs, td_error = self.optimise_qnetwork()
        logs = logs | self.target_updater.update(time_step)
        if isinstance(self.memory, PrioritizedMemory):
            logs = logs | self.memory.update(td_error)
        return logs

    def _can_update(self):
        return self.memory.can_sample(self.batch_size)

    def _next_state_value(self, batch: Batch):
        # We use the all_obs_ to handle the case of recurrent qnetworks that require the first element of the sequence.
        next_qvalues = self.qtarget.forward(batch.next_obs)
        # For double q-learning, we use the qnetwork to select the best action. Otherwise, we use the target qnetwork.
        if self.double_qlearning:
            qvalues_for_index = self.qnetwork.forward(batch.next_obs)
        else:
            qvalues_for_index = next_qvalues
        qvalues_for_index[batch.next_available_actions == 0.0] = -torch.inf
        indices = torch.argmax(qvalues_for_index, dim=-1, keepdim=True)
        next_values = torch.gather(next_qvalues, -1, indices).squeeze(-1)
        return next_values

    def optimise_qnetwork(self):
        batch = self.memory.sample(self.batch_size)

        # Qvalues and qvalues with target network computation
        qvalues: torch.Tensor = self.qnetwork.forward(batch.obs)
        qvalues = torch.gather(qvalues, dim=-1, index=batch.actions)
        qvalues = qvalues.squeeze(-1)

        # Next state value computation
        # We use the all_obs_ to handle the case of recurrent qnetworks that require the first element of the sequence.
        next_values = self._next_state_value(batch)
        qtargets = batch.rewards + self.gamma * next_values * (1 - batch.dones)
        # Compute the loss
        td_error = qvalues - qtargets.detach()
        loss = torch.mean(td_error**2)
        # Optimize
        logs = {"loss": float(loss.item())}
        self.optimiser.zero_grad()
        loss.backward()
        if self.grad_norm_clipping is not None:
            grad_norm = torch.nn.utils.clip_grad_norm_(self.qnetwork.parameters(), self.grad_norm_clipping)
            logs["grad_norm"] = grad_norm.item()
        self.optimiser.step()
        return logs, td_error
