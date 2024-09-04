from typing import Optional

import numpy as np
import torch
from gnn import Actor, Critic
from optimenv import Observation
from replay_memory import ReplayMemory

from problem import Problem
from .algo import Algo


class PPO(Algo):
    def __init__(
        self,
        problem: Problem,
        gamma: float,
        batch_size: int,
        update_interval: int,
        n_epochs: int,
        c1: float,
        c2: float,
        device: Optional[torch.device] = None,
        logits_clip_low: float = -10,
        logits_clip_high: float = 10,
    ):
        super().__init__()
        if device is None:
            device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
        self.device = device
        self.gamma = gamma
        self.batch_size = batch_size
        self.update_interval = update_interval
        self.n_epochs = n_epochs
        self.c1 = c1
        self.c2 = c2

        self.actor = Actor(problem).to(self.device)
        self.critic = Critic(problem).to(self.device)
        self.memory = ReplayMemory(200)

        self.logits_clip_low = logits_clip_low
        self.logits_clip_high = logits_clip_high

    def select_action(self, obs: Observation):
        with torch.no_grad():
            obs_data = obs.graph.to(self.device.index, non_blocking=True)
            logits = self.actor.pi(obs_data)
            distribution = torch.distributions.Categorical(logits)
            action = distribution.sample().item()
            return int(action), logits.numpy(force=True)

    def learn(self, time_step: int, obs: Observation, action: int, reward: float, next_obs: Observation) -> dict[str, float]:
        self.memory.add(obs, action, reward, next_obs)

        return {}

    def actions_logits(self, obs: Observation):
        obs.graph = obs.graph.to(self.device.index, non_blocking=True)
        logits = self.actor.pi(obs.graph)
        logits[torch.tensor(obs.available_actions) == 0] = -torch.inf  # mask unavailable actions
        return logits

    def to(self, device: torch.device):
        self.actor = self.actor.to(device, non_blocking=True)
        self.critic = self.critic.to(device, non_blocking=True)
        self.device = device
