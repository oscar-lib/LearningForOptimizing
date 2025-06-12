from typing import Sequence
import torch
import torch_geometric.nn as gnn
import math
from torch_geometric.data import Data

from problem import PDPTW, CSP


class QNetGNN(torch.nn.Module):
    def __init__(self, problem: PDPTW):
        super().__init__()
        # Node convolutions
        self.OUT_NODE_FEATURES = 64
        self.node_conv1 = gnn.GCNConv(problem.n_node_features, 32)
        self.lrelu = torch.nn.LeakyReLU(0.1)
        self.node_conv2 = gnn.GCNConv(32, self.OUT_NODE_FEATURES)

        # Edge convolution, currently not used
        self.OUT_EDGE_FEATURES = 32
        self.edge_conv = gnn.EdgeConv(
            torch.nn.Sequential(
                torch.nn.Linear(problem.n_edge_features * 2, problem.n_edge_features),
                torch.nn.LeakyReLU(0.1),
                torch.nn.Linear(problem.n_edge_features, self.OUT_EDGE_FEATURES),
            )
        )

        self.linear = torch.nn.Sequential(
            torch.nn.Linear(self.OUT_NODE_FEATURES, 64),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(64, problem.n_actions),
        )

    def forward(self, data: Data) -> torch.Tensor:
        assert data.x is not None
        node_x = self.node_conv1.forward(data.x, data.edge_index)
        node_x = self.lrelu(node_x)
        node_x = self.node_conv2.forward(node_x, data.edge_index)
        node_x = gnn.pool.global_max_pool(node_x, batch=data.batch)
        x = self.linear.forward(node_x)
        return x


class Actor(torch.nn.Module):
    def __init__(self, problem: PDPTW, n_out_features: int = 64):
        super().__init__()

        self.node_conv1 = gnn.GCNConv(problem.n_node_features, 32)
        self.lrelu = torch.nn.LeakyReLU(0.1)
        self.node_conv2 = gnn.GCNConv(32, n_out_features)

        self.linear = torch.nn.Sequential(
            torch.nn.Linear(n_out_features, 128),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(128, 64),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(64, problem.n_actions),
        )

    def forward(self, data: Data) -> torch.Tensor:
        assert data.x is not None
        node_x = self.node_conv1.forward(data.x, data.edge_index)
        node_x = self.lrelu(node_x)
        node_x = self.node_conv2.forward(node_x, data.edge_index)
        node_x = gnn.pool.global_max_pool(node_x, batch=data.batch)
        x = self.linear.forward(node_x)
        return x


class Critic(torch.nn.Module):
    def __init__(self, problem: PDPTW, n_out_features: int = 64):
        super(Critic, self).__init__()
        self.node_conv1 = gnn.GCNConv(problem.n_node_features, 32)
        self.lrelu = torch.nn.LeakyReLU(0.1)
        self.node_conv2 = gnn.GCNConv(32, n_out_features)

        self.linear = torch.nn.Sequential(
            torch.nn.Linear(n_out_features, 128),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(128, 64),
            torch.nn.LeakyReLU(0.1),
            torch.nn.Linear(64, 1),
        )

    def forward(self, data: Data) -> torch.Tensor:
        assert data.x is not None
        node_x = self.node_conv1.forward(data.x, data.edge_index)
        node_x = self.lrelu(node_x)
        node_x = self.node_conv2.forward(node_x, data.edge_index)
        node_x = gnn.pool.global_max_pool(node_x, batch=data.batch)
        x = self.linear.forward(node_x)
        return x


class CNN(torch.nn.Module):
    def __init__(self, problem: CSP):
        super().__init__()
        self.problem = problem
        self.cars_data = problem.cars_data.unsqueeze(0).unsqueeze(0)  # Add channel and batch dimensions
        self.options_data = problem.options_data.flatten().unsqueeze(0)  # Add channel and batch dimensions
        n_common_inputs = math.prod(self.options_data.shape)
        self.instance_extractor, n_outputs = make_cnn(self.cars_data.shape[1:], [32, 64, 32], [3, 3, 3], [1, 1, 1])
        n_common_inputs += n_outputs
        state_shape = (1, problem.n_options, problem.n_cars)
        self.state_extractor, n_outputs = make_cnn(state_shape, [32, 64, 32], [3, 3, 3], [1, 1, 1])
        n_common_inputs += n_outputs
        layer_size = n_common_inputs
        layers = []
        while layer_size > 128:
            layers.append(torch.nn.Linear(layer_size, layer_size // 2))
            layers.append(torch.nn.ReLU())
            layer_size //= 2
        layers.append(torch.nn.Linear(layer_size, problem.n_actions))
        self.common = torch.nn.Sequential(*layers)

    def forward(self, current_solution: torch.Tensor) -> torch.Tensor:
        batch_size, *_ = current_solution.shape
        # Repeat the cars data for each batch element
        cars_data = self.cars_data.repeat(batch_size, 1, 1, 1)
        options_data = self.options_data.repeat(batch_size, 1)
        x1 = self.instance_extractor.forward(cars_data)
        x2 = self.state_extractor.forward(current_solution)
        x = torch.cat((x1, x2, options_data), dim=1)
        qvalues = self.common.forward(x)
        return qvalues


def make_cnn(
    input_shape: Sequence[int],
    filters: Sequence[int],
    kernel_sizes: Sequence[int],
    strides: Sequence[int],
    min_output_size=1024,
):
    """Create a CNN with flattened output based on the given filters, kernel sizes and strides."""
    channels, height, width = input_shape
    paddings = [0 for _ in filters]
    n_padded = 0
    output_w, output_h = conv2d_size_out(width, height, kernel_sizes, strides, paddings)
    output_size = filters[-1] * output_w * output_h
    while output_w <= 1 or output_h <= 1 or output_size < min_output_size:
        # Add paddings if the output size is negative
        paddings[n_padded % len(paddings)] += 1
        n_padded += 1
        output_w, output_h = conv2d_size_out(width, height, kernel_sizes, strides, paddings)
        output_size = filters[-1] * output_w * output_h
    assert output_h > 0 and output_w > 0, f"Input size = {input_shape}, output witdh = {output_w}, output height = {output_h}"
    modules = []
    for f, k, s, p in zip(filters, kernel_sizes, strides, paddings):
        modules.append(torch.nn.Conv2d(in_channels=channels, out_channels=f, kernel_size=k, stride=s, padding=p))
        modules.append(torch.nn.ReLU())
        channels = f
    modules.append(torch.nn.Flatten())
    return torch.nn.Sequential(*modules), output_size


def conv2d_size_out(input_width: int, input_height: int, kernel_sizes: Sequence[int], strides: Sequence[int], paddings: Sequence[int]):
    """
    Compute the output width and height of a sequence of 2D convolutions.
    See shape section on https://pytorch.org/docs/stable/generated/torch.nn.Conv2d.html
    """
    width = input_width
    height = input_height
    for kernel_size, stride, pad in zip(kernel_sizes, strides, paddings):
        width = (width + 2 * pad - (kernel_size - 1) - 1) // stride + 1
        height = (height + 2 * pad - (kernel_size - 1) - 1) // stride + 1
    return width, height
