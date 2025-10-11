from collections.abc import Iterator
import math
from abc import ABC, abstractmethod
from typing import Sequence

import torch
from torch.nn.parameter import Parameter
import torch_geometric.nn as gnn
from problem import CSP, PDPTW, TSP
from torch import distributions
from torch_geometric.data import Data


class ActorCritic(torch.nn.Module, ABC):
    def __init__(self, n_actions: int):
        super().__init__()
        self.n_actions = n_actions
        # n_actions for the means
        # n_actions ** 2 for the covariance matrix
        self.output_size = n_actions + n_actions**2

    @abstractmethod
    def policy(self, states: torch.Tensor | Data) -> torch.distributions.Categorical: ...

    @abstractmethod
    def value(self, states: torch.Tensor | Data) -> torch.Tensor: ...

    @abstractmethod
    def actor_parameters(self) -> list[torch.nn.Parameter]: ...

    @abstractmethod
    def critic_parameters(self) -> list[torch.nn.Parameter]: ...


class QNetGNN(torch.nn.Module):
    def __init__(self, problem: PDPTW | TSP):
        super().__init__()
        # Node convolutions
        self.qnetwork = GNN(problem, problem.n_actions, 64)

    def forward(self, data: Data) -> torch.Tensor:
        return self.qnetwork.forward(data)


class CSPActorCritic(ActorCritic):
    def __init__(self, problem: CSP):
        super().__init__(problem.n_actions)
        self.actor = CSPNetwork(problem, problem.n_actions)
        self.critic = CSPNetwork(problem, 1)

    def policy(self, states: torch.Tensor) -> torch.distributions.Categorical:
        x = self.actor.forward(states)
        return distributions.Categorical(logits=x)

    def value(self, states: torch.Tensor) -> torch.Tensor:
        return self.critic.forward(states).squeeze(-1)

    def actor_parameters(self) -> list[torch.nn.Parameter]:
        return list(self.actor.parameters())

    def critic_parameters(self) -> list[torch.nn.Parameter]:
        return list(self.critic.parameters())

    def to(self, device: torch.device, *args, non_blocking: bool = True, **kwargs):
        """Override to ensure the CNN is moved to the correct device."""
        self.actor.to(device, non_blocking=non_blocking, *args, **kwargs)
        self.critic.to(device, non_blocking=non_blocking, *args, **kwargs)
        return super().to(device, non_blocking=non_blocking, *args, **kwargs)


class CSPNetwork(torch.nn.Module):
    def __init__(self, problem: CSP, n_network_outputs: int):
        super().__init__()
        self.options_data = problem.options_data.flatten().unsqueeze(0)  # Add channel and batch dimensions
        max_seq_length = problem.max_seq_length
        self.cnn = torch.nn.Sequential(
            torch.nn.Conv1d(problem.n_options, 32, kernel_size=max_seq_length, stride=1),  # out_1 = problem.n_cars - max_seq_length + 1
            torch.nn.LeakyReLU(),
            torch.nn.Conv1d(32, 64, kernel_size=max_seq_length, stride=1),  # out2 = out_1 - max_seq_length + 1
            torch.nn.LeakyReLU(),
            torch.nn.Conv1d(64, 16, kernel_size=max_seq_length, stride=1),  # out3 = out2 - max_seq_length + 1
            torch.nn.LeakyReLU(),
            torch.nn.Flatten(),
        )
        n_outputs = (problem.n_cars - (max_seq_length - 1) * 3) * 16
        layers = []
        layer_size = n_outputs + math.prod(self.options_data.shape)
        while layer_size > 192:
            next_layer_size = layer_size // 2
            layers.append(torch.nn.Linear(layer_size, next_layer_size))
            layers.append(torch.nn.ReLU())
            layer_size = next_layer_size
        layers.append(torch.nn.Linear(layer_size, n_network_outputs))
        self.linear = torch.nn.Sequential(*layers)

    def forward(self, solution: torch.Tensor) -> torch.Tensor:
        """
        solution: Tensor of shape (batch_size, n_cars, n_options)
        """
        batch_size, *_ = solution.shape
        x = self.cnn(solution)
        options_data = self.options_data.repeat(batch_size, 1)
        x = torch.cat((x, options_data), dim=1)
        return self.linear(x)

    def to(self, device: torch.device, *args, non_blocking: bool = True, **kwargs):
        """Override to ensure the CNN is moved to the correct device."""
        self.options_data = self.options_data.to(device, non_blocking=non_blocking)
        return super().to(device, non_blocking=non_blocking, *args, **kwargs)


def make_cnn(
    input_shape: tuple[int, int],
    filters: Sequence[int],
    kernel_sizes: Sequence[int],
    strides: Sequence[int],
):
    """Create a CNN with flattened output based on the given filters, kernel sizes and strides."""
    channels, width = input_shape
    layers = []
    for ks, s, fs in zip(kernel_sizes, strides, filters):
        layers.append(torch.nn.Conv1d(in_channels=channels, out_channels=fs, kernel_size=ks, stride=s, padding=0))
        width = conv1d_output_width(width, ks, s)
        channels = fs
        layers.append(torch.nn.ReLU())
    layers.append(torch.nn.Flatten())
    return torch.nn.Sequential(*layers), channels * width


def conv1d_output_width(input_width: int, kernel_size: int, stride: int, padding: int = 0, dilatation: int = 1) -> int:
    """
    Compute the output width of a sequence of 1D convolutions.
    See shape section on https://pytorch.org/docs/stable/generated/torch.nn.Conv1d.html
    """
    return (input_width + 2 * padding - dilatation * (kernel_size - 1) - 1) // stride + 1


class ActorCriticGNN(ActorCritic):
    def __init__(self, problem: PDPTW | TSP, n_out_features: int = 64):
        super().__init__(problem.n_actions)

        self.actor = GNN(problem, problem.n_actions, n_out_features)
        self.critic = GNN(problem, 1, n_out_features)

    def policy(self, data: Data):
        x = self.actor.forward(data)
        return distributions.Categorical(logits=x)

    def value(self, states: Data):
        return self.critic.forward(states).squeeze(-1)

    def actor_parameters(self) -> list[Parameter]:
        return list(self.actor.parameters())

    def critic_parameters(self) -> list[Parameter]:
        return list(self.critic.parameters())


class GNN(torch.nn.Module):
    def __init__(self, problem: PDPTW | TSP, n_outputs: int, n_out_features: int = 64):
        super().__init__()
        self.node_conv1 = gnn.GCNConv(problem.n_node_features, 32)
        self.lrelu = torch.nn.LeakyReLU(0.1)
        self.node_conv2 = gnn.GCNConv(32, n_out_features)

        self.linear = torch.nn.Sequential(
            torch.nn.Linear(n_out_features, 128),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(128, 64),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(64, n_outputs),
        )

    def forward(self, data: Data) -> torch.Tensor:
        assert data.x is not None
        node_x = self.node_conv1.forward(data.x, data.edge_index)
        node_x = self.lrelu(node_x)
        node_x = self.node_conv2.forward(node_x, data.edge_index)
        node_x = gnn.pool.global_max_pool(node_x, batch=data.batch)
        x = self.linear.forward(node_x)
        return x
