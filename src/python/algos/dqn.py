from copy import deepcopy
from dataclasses import dataclass
from typing import Optional

import torch
from optimenv import Observation
from policies import EpsilonGreedy
from problem import Problem
from qtarget_updater import HardUpdate
from replay_memory.prioritized_memory import PrioritizedMemory
from replay_memory.replay_memory import Batch, ReplayMemory

from .algo import Algo


@dataclass
class DQN(Algo):
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
        epsilon: float = 0.1,
        grad_norm_clipping: Optional[float] = None,
        double_qlearning: bool = False,
        device: Optional[torch.device] = None,
    ):
        super().__init__()
        if device is None:
            device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
        self.device = device
        self.qnetwork = qnetwork.to(device, non_blocking=True)
        self.qtarget = deepcopy(qnetwork).to(device, non_blocking=True)
        self.memory = memory
        self.gamma = gamma
        self.batch_size = batch_size
        self.double_qlearning = double_qlearning
        self.policy = EpsilonGreedy.constant(epsilon)
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

    def select_action(self, obs: Observation):
        obs.graph = obs.graph.to(self.device.index, non_blocking=True)
        qvalues = self.compute_qvalues(obs).squeeze().numpy(force=True)
        action = self.policy.get_action(qvalues, obs.available_actions)
        return action, qvalues

    def compute_qvalues(self, state: Observation) -> torch.Tensor:
        return self.qnetwork.forward(state.graph)

    def notify_episode_end(self):
        self.memory.end_episode()

    def learn(self, time_step: int, obs: Observation, action: int, reward: float, next_obs: Observation) -> dict[str, float]:
        next_obs.graph = next_obs.graph.to(self.device.index, non_blocking=True)
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
        indices = torch.argmax(qvalues_for_index, dim=-1, keepdim=True)
        next_values = torch.gather(next_qvalues, -1, indices).squeeze(-1)
        return next_values

    def optimise_qnetwork(self):
        batch = self.memory.sample(self.batch_size).to(self.device)

        # Qvalues and qvalues with target network computation
        qvalues = self.qnetwork.forward(batch.obs)
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

    def to(self, device: torch.device):
        self.device = device
        self.qnetwork = self.qnetwork.to(device, non_blocking=True)
        self.qtarget = self.qtarget.to(device, non_blocking=True)
        return self
