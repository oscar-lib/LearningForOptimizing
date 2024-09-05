import torch
import torch_geometric.nn as gnn
import torch.nn.functional as F
from torch_geometric.data import Data

from problem import Problem


class QNetGNN(torch.nn.Module):
    def __init__(self, problem: Problem):
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
    def __init__(self, problem: Problem, n_out_features: int = 64):
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
    def __init__(self, problem: Problem, n_out_features: int = 64):
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
