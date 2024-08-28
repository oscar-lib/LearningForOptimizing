import torch
import torch_geometric.nn as gnn

from .problem import Problem


class QNetGNN(torch.nn.Module):
    def __init__(self, problem: Problem, n_actions: int):
        super().__init__()
        # Each node has 5 features:
        # - x, y position
        # - x, y delivery
        # - earliest pickup time
        # - latest pickup time
        # - pickup duration
        # - quantity
        self.network = torch.nn.Sequential(
            gnn.GCNConv(problem.n_node_features, 16),
            torch.nn.ReLU(),
            gnn.GCNConv(16, n_actions),
        )
