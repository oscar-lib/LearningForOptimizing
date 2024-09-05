from dataclasses import dataclass

import numpy as np
from .algo import Algo
import torch
from gnn import Actor, Critic
from optimenv import Observation
from problem import Problem
from torch.distributions import Categorical
from torch_geometric.data import Data
from torch_geometric.loader import DataLoader


@dataclass
class Batch:
    obs: Data
    available_actions: torch.Tensor
    actions: torch.Tensor
    rewards: torch.Tensor
    dones: torch.Tensor
    size: int

    def normalized_returns(self, gamma: float):
        """(normalized) Monte Carlo estimate of the state values (returns)."""
        returns = []
        discounted_reward = 0
        for reward, is_terminal in zip(reversed(self.rewards), reversed(self.dones)):
            if is_terminal:
                discounted_reward = 0
            discounted_reward = reward + (gamma * discounted_reward)
            returns.insert(0, discounted_reward)

        # Normalizing the returns
        returns = torch.tensor(returns, dtype=torch.float32)
        returns = (returns - returns.mean()) / (returns.std() + 1e-7)
        return returns

    def to(self, device: torch.device) -> "Batch":
        return Batch(
            obs=self.obs.to(device.index, non_blocking=True),
            available_actions=self.available_actions.to(device),
            actions=self.actions.to(device, non_blocking=True),
            rewards=self.rewards.to(device, non_blocking=True),
            dones=self.dones.to(device, non_blocking=True),
            size=self.size,
        )

    def tmp(self, gamma: float):
        result = torch.zeros_like(self.rewards, dtype=torch.float32)
        next_step_returns = self.rewards[-1]
        result[-1] = next_step_returns
        for step in range(self.size - 2, -1, -1):
            next_step_returns = self.rewards[step] + gamma * next_step_returns
            result[step] = next_step_returns
        return result


class RolloutBuffer:
    def __init__(self):
        self.actions = list[int]()
        self.obs = list[Observation]()
        self.rewards = list[float]()
        self.dones = list[bool]()

    def store_action(self, obs: Observation, action: int):
        self.obs.append(obs)
        self.actions.append(action)
        self.dones.append(False)

    def store_reward(self, reward: float, next_obs: Observation):
        self.rewards.append(reward)

    def end_episode(self):
        self.dones[-1] = True
        self.rewards.append(0)

    def clear(self):
        self.actions = []
        self.obs = []
        self.rewards = []
        self.values = []
        self.dones = []

    def sample(self):
        batch_size = len(self.actions)
        obs = DataLoader([obs.graph for obs in self.obs], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        actions = torch.tensor(self.actions, dtype=torch.long)
        rewards = torch.tensor(self.rewards, dtype=torch.float32)
        dones = torch.tensor(self.dones, dtype=torch.bool)
        available_actions = torch.stack([obs.available_actions for obs in self.obs])
        return Batch(
            obs=obs,
            available_actions=available_actions,
            actions=actions,
            rewards=rewards,
            dones=dones,
            size=batch_size,
        )

    def __len__(self):
        return len(self.actions)


class PPO(Algo):
    def __init__(
        self,
        problem: Problem,
        lr_actor,
        lr_critic,
        gamma,
        K_epochs,
        eps_clip,
        batch_size=32,
        device: torch.device | None = None,
        c1: float = 0.5,
        c2: float = 0.01,
    ):
        super().__init__()
        self.gamma = gamma
        self.eps_clip = eps_clip
        self.K_epochs = K_epochs
        self.c1 = c1
        self.c2 = c2
        self.batch_size = batch_size

        if device is None:
            device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
        self.device = device

        self.buffer = RolloutBuffer()
        self.actor = Actor(problem)
        self.critic = Critic(problem)
        self.optimizer = torch.optim.Adam(  # type: ignore
            [
                {"params": self.actor.parameters(), "lr": lr_actor},
                {"params": self.critic.parameters(), "lr": lr_critic},
            ]
        )
        self.mse_loss = torch.nn.MSELoss()

    @staticmethod
    def default(problem: Problem):
        return PPO(
            problem=problem,
            lr_actor=0.001,
            lr_critic=0.001,
            gamma=0.99,
            K_epochs=20,
            eps_clip=0.2,
        )

    def select_action(self, obs: Observation):
        with torch.no_grad():
            obs.graph = obs.graph.to(self.device.index, non_blocking=True)
            obs.available_actions = obs.available_actions.to(self.device, non_blocking=True)
            logits = self.actor.forward(obs.graph).squeeze()
            # mask unavailable actions
            logits[~obs.available_actions] = -torch.inf
            dist = Categorical(logits=logits)
            action = int(dist.sample().item())

        self.buffer.store_action(obs, action)
        return action, logits.numpy(force=True)

    def learn(self, time_step: int, obs: Observation, action: int, reward: float, next_obs: Observation) -> dict[str, float]:
        self.buffer.store_reward(reward, next_obs)
        if len(self.buffer) < self.batch_size:
            return {}
        return self.update(next_obs)

    def notify_episode_end(self):
        self.buffer.end_episode()

    def update(self, next_obs: Observation):
        batch = self.buffer.sample().to(self.device)
        # next_state_value = self.critic.forward(next_obs.graph).squeeze().item()
        returns = batch.normalized_returns(self.gamma).to(self.device)
        with torch.no_grad():
            old_values = self.critic.forward(batch.obs).squeeze()
            old_logits = self.actor.forward(batch.obs)
            old_log_probs = Categorical(logits=old_logits).log_prob(batch.actions)
            advantages = returns - old_values
        total_loss = 0
        # Optimize policy for K epochs
        for _ in range(self.K_epochs):
            # Evaluating old actions and values
            logits = self.actor.forward(batch.obs)
            dist = Categorical(logits=logits)

            log_probs = dist.log_prob(batch.actions)
            state_values = self.critic.forward(batch.obs).squeeze()

            # Finding the ratio (pi_theta / pi_theta__old)
            ratios = torch.exp(log_probs - old_log_probs)
            dist_entropy = dist.entropy()
            # Finding Surrogate Loss
            surr1 = ratios * advantages
            surr2 = torch.clamp(ratios, 1 - self.eps_clip, 1 + self.eps_clip) * advantages

            # final loss of clipped objective PPO
            actor_loss = -torch.min(surr1, surr2)
            critic_loss = self.mse_loss.forward(state_values, returns)
            loss = torch.mean(actor_loss + self.c1 * critic_loss - self.c2 * dist_entropy)

            # take gradient step
            self.optimizer.zero_grad()
            loss.backward()
            self.optimizer.step()
            total_loss += loss.item()

        self.buffer.clear()
        return {"avg-loss": total_loss / self.K_epochs}

    def to(self, device: torch.device):
        self.actor.to(device, non_blocking=True)
        self.critic.to(device, non_blocking=True)
        self.device = device
        return self
