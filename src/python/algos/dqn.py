from dataclasses import dataclass
from typing import Optional
import os
from copy import deepcopy
import torch
from torch_geometric.data import Data
from optimenv import Observation
from policies import EpsilonGreedy
from qtarget_updater import HardUpdate
from replay_memory.replay_memory import Batch, ReplayMemory
import random

from .algo import Algo


@dataclass
class DQN(Algo):
    qnetwork: torch.nn.Module
    memory: ReplayMemory
    gamma: float
    batch_size: int
    grad_norm_clipping: Optional[float]
    lr: float
    policy: EpsilonGreedy

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
        no_target: bool = False,
        enable_logs: bool = True,
    ):
        super().__init__()
        self.device = torch.device("cpu")
        self.qnetwork = qnetwork
        self.qtarget = deepcopy(qnetwork)
        self.memory = memory
        self.gamma = gamma
        self.batch_size = batch_size
        self.double_qlearning = double_qlearning
        # self.policy = EpsilonGreedy.constant(epsilon)
        self.epsilon = epsilon
        self.lr = lr
        self.optimiser = torch.optim.Adam(self.qnetwork.parameters(), lr=lr)
        # Parameters and optimiser
        self.grad_norm_clipping = grad_norm_clipping
        self.target_updater = HardUpdate(update_period=100)
        self.use_target = not no_target
        self.parameters = list(self.qnetwork.parameters())
        self.enable_logs = enable_logs

    def select_action(self, obs: Observation[torch.Tensor | Data]):
        # Avoid forward pass if we take a random action
        if random.random() < self.epsilon:
            available = [i for i, v in enumerate(obs.available_actions) if v]
            return random.choice(available), []
        if isinstance(obs.data, torch.Tensor):
            data = obs.data.unsqueeze(0)  # Add batch dimension
        else:
            data = obs.data
        qvalues: torch.Tensor = self.qnetwork.forward(data).squeeze(0)  # Squeeze the batch dimension
        action = int(qvalues.argmax())
        return action, qvalues.tolist()

    def notify_episode_end(self):
        self.memory.end_episode()

    def learn(
        self,
        time_step: int,
        obs: Observation[torch.Tensor],
        action: int,
        reward: float,
        next_obs: Observation,
        next_obs_value: float,
    ) -> dict[str, float]:
        self.memory.add(obs, action, reward, next_obs, next_obs_value)
        if not self._can_update():
            return {}
        logs, td_error = self.optimise_qnetwork()
        if self.use_target:
            logs = logs | self.target_updater.update(time_step)
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
        # Sample a batch from the memory
        batch = self.memory.sample(self.batch_size).to(self.device)
        # Qvalues and qvalues with target network computation
        qvalues = self.qnetwork.forward(batch.obs)
        qvalues = torch.gather(qvalues, dim=-1, index=batch.actions)
        qvalues = qvalues.squeeze(-1)

        # Next state value computation
        if self.use_target:
            next_values = self._next_state_value(batch).detach()
        else:
            next_values = batch.next_values
        qtargets = batch.rewards + self.gamma * next_values * (~batch.dones)
        # Compute the loss
        td_error = qvalues - qtargets
        loss = torch.mean(td_error**2)
        # Optimize
        logs = dict[str, float]()
        if self.enable_logs:
            logs["loss"] = loss.item()
        self.optimiser.zero_grad()
        loss.backward()
        if self.grad_norm_clipping is not None:
            grad_norm = torch.nn.utils.clip_grad_norm_(self.parameters, self.grad_norm_clipping)
            if self.enable_logs:
                logs["grad_norm"] = grad_norm.item()
        self.optimiser.step()
        return logs, td_error

    def to(self, device: torch.device):
        self.device = device
        self.qnetwork = self.qnetwork.to(device, non_blocking=True)
        self.qtarget = self.qtarget.to(device, non_blocking=True)
        return self

    def save(self, directory: str):
        os.makedirs(directory, exist_ok=True)
        torch.save(self.qnetwork.state_dict(), os.path.join(directory, "dqn.weights"))

    def load(self, directory: str):
        weights = torch.load(os.path.join(directory, "dqn.weights"))
        self.qnetwork.load_state_dict(weights)
        self.optimiser = torch.optim.Adam(self.qnetwork.parameters(), lr=self.lr)
