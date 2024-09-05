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
    values: torch.Tensor
    log_probs: torch.Tensor

    def returns(self, gamma: float):
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

    def tmp(self, gamma: float):
        result = torch.zeros_like(self.rewards, dtype=torch.float32)
        next_step_returns = self.rewards[-1]
        result[-1] = next_step_returns
        for step in range(self.size - 2, -1, -1):
            next_step_returns = self.rewards[step] + gamma * next_step_returns
            result[step] = next_step_returns
        return result

    def advantages(self, returns: torch.Tensor):
        return returns - self.values


class RolloutBuffer:
    def __init__(self):
        self.actions = list[int]()
        self.obs = list[Observation]()
        self.logprobs = list[float]()
        self.rewards = list[float]()
        self.values = list[float]()
        self.dones = list[bool]()

    def store_action(self, obs: Observation, action: int, action_logprob: float, value: float):
        self.obs.append(obs)
        self.actions.append(action)
        self.logprobs.append(action_logprob)
        self.values.append(value)
        self.dones.append(False)

    def store_reward(self, reward: float, next_obs: Observation):
        self.rewards.append(reward)

    def end_episode(self):
        self.dones[-1] = True

    def clear(self):
        self.actions = []
        self.obs = []
        self.logprobs = []
        self.rewards = []
        self.values = []
        self.dones = []

    def sample(self):
        batch_size = len(self.actions)
        obs = DataLoader([obs.graph for obs in self.obs], batch_size=batch_size, shuffle=False)._get_iterator().__next__()
        actions = torch.tensor(self.actions, dtype=torch.long)
        rewards = torch.tensor(self.rewards, dtype=torch.float32)
        dones = torch.tensor(self.dones, dtype=torch.bool)
        values = torch.tensor(self.values, dtype=torch.float32)
        available_actions = torch.from_numpy(np.array([obs.available_actions for obs in self.obs], dtype=bool))
        return Batch(
            obs=obs,
            available_actions=available_actions,
            actions=actions,
            rewards=rewards,
            dones=dones,
            size=batch_size,
            values=values,
            log_probs=torch.tensor(self.logprobs, dtype=torch.float32),
        )


class PPO(Algo):
    def __init__(
        self,
        problem: Problem,
        lr_actor,
        lr_critic,
        gamma,
        K_epochs,
        eps_clip,
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
            K_epochs=4,
            eps_clip=0.2,
        )

    def select_action(self, obs: Observation):
        with torch.no_grad():
            obs.graph = obs.graph.to(self.device.index)
            log_probs = self.actor.forward(obs.graph)
            value = self.critic.forward(obs.graph)

            # mask unavailable actions
            log_probs[obs.available_actions == 0] = -torch.inf
            dist = Categorical(log_probs)
            action = dist.sample()
            action_logprob = dist.log_prob(action).item()
            action = int(action.item())

        self.buffer.store_action(obs, action, action_logprob, value.item())
        return action, log_probs.numpy(force=True)

    def learn(self, time_step: int, obs: Observation, action: int, reward: float, next_obs: Observation) -> dict[str, float]:
        self.buffer.store_reward(reward, next_obs)
        return {}

    def notify_episode_end(self):
        self.buffer.end_episode()

    def update(self):
        batch = self.buffer.sample()
        returns = batch.returns(self.gamma)
        advantages = batch.advantages(returns)
        # Optimize policy for K epochs
        for _ in range(self.K_epochs):
            # Evaluating old actions and values
            logprobs = self.actor.forward(batch.obs)
            dist_entropy = Categorical(logits=logprobs).entropy()
            state_values = self.critic.forward(batch.obs)

            # match state_values tensor dimensions with rewards tensor
            state_values = torch.squeeze(state_values)

            # Finding the ratio (pi_theta / pi_theta__old)
            ratios = torch.exp(logprobs - batch.log_probs)

            # Finding Surrogate Loss
            surr1 = ratios * advantages
            surr2 = torch.clamp(ratios, 1 - self.eps_clip, 1 + self.eps_clip) * advantages

            # final loss of clipped objective PPO
            actor_loss = -torch.min(surr1, surr2)
            critic_loss = self.mse_loss.forward(state_values, returns)
            loss = actor_loss + self.c1 * critic_loss - self.c2 * dist_entropy

            # take gradient step
            self.optimizer.zero_grad()
            loss.mean().backward()
            self.optimizer.step()

        # clear buffer
        self.buffer.clear()

    def to(self, device: torch.device):
        self.actor.to(device, non_blocking=True)
        self.critic.to(device, non_blocking=True)
        self.device = device
        return self
