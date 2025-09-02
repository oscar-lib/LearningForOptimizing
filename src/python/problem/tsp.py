from dataclasses import dataclass
from typing import Self

import orjson
import torch
from torch_geometric.data import Data

from .problem import Problem


@dataclass
class Node:
    num: int
    coords: list[float]
    is_depot: bool = False

    def get_data(self, coord_max: list[float]):
        return [c / m for c, m in zip(self.coords, coord_max)]


class TSP(Problem):
    def __init__(self, n_actions: int, nodes: list[Node]):
        super().__init__()
        self.nodes = nodes
        self.n_actions = n_actions
        n_coords = len(nodes[0].coords)
        max_coords = [0.0] * n_coords
        for i in range(n_coords):
            max_coords[i] = max(node.coords[i] for node in nodes)
        self.node_data = torch.tensor([node.get_data(max_coords) for node in nodes])

    @property
    def n_node_features(self):
        return self.node_data.size(1)

    @property
    def n_nodes(self):
        return len(self.nodes)

    @property
    def n_edge_features(self):
        return 0

    @staticmethod
    def parse(bdata: bytes):
        data: dict = orjson.loads(bdata)
        coords = data["problem"]["cityCoords"]
        n_coords = len(coords[0])
        # We compute the minimum values for each coordinate to normalize the input such that there is
        # no negative value for any coordinate
        mins = [min(coord[i] for coord in coords) for i in range(n_coords)]
        nodes = [Node(0, mins.copy(), is_depot=True)]
        for coord in coords:
            coord = [c - m for c, m in zip(coord, mins)]
            nodes.append(Node(num=len(nodes), coords=coord))
        return TSP(data["nActions"], nodes)

    def build_agent_input(self, data: dict, device: torch.device):
        from .pdptw import PDPTW

        routes = data["state"]
        edges = PDPTW._compute_edges(routes)
        graph = Data(self.node_data, edges)
        graph.validate()
        return graph.to(device.index, non_blocking=True)

    def is_compatible_with(self, other: Self) -> bool:
        if self.n_nodes != other.n_nodes:
            return False
        if self.n_actions != other.n_actions:
            return False
        return self.n_node_features == other.n_node_features
