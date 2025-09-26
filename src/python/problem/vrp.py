from typing import Self
import torch
import numpy as np
from functools import cached_property
from torch_geometric.data import Data
from .problem import Problem
from abc import abstractmethod


Coord = tuple[float, ...]
NodeID = int


class VRPNode:
    index: NodeID
    """ID of the node, index in the distance vector"""
    coords: Coord
    """Coordinates of the node"""
    distance_vector: list[float]
    """Distances from this node to all nodes (including itself)"""

    def __init__(self, index: int, coords: Coord, distance_vector: list[float]):
        self.index = index
        self.coords = coords
        self.distance_vector = distance_vector

    @cached_property
    def attrs(self):
        """Node attributes for the neural network input"""
        return [*self.coords, *self.distance_vector]

    @property
    def n_dims(self) -> int:
        return len(self.coords)

    def distance(self, other: "VRPNode") -> float:
        return sum((c1 - c2) ** 2 for c1, c2 in zip(self.coords, other.coords)) ** 0.5

    def vector_to(self, other: "VRPNode"):
        return [(c2 - c1) for c1, c2 in zip(other.coords, self.coords)]


class VRP[N: VRPNode](Problem[Data]):
    n_vehicles: int
    nodes: list[N]
    n_actions: int

    def __init__(self, n_vehicle: int, nodes: list[N], n_actions: int):
        super().__init__()
        self.n_vehicles = n_vehicle
        self.nodes = nodes
        self.n_actions = n_actions
        self.node_data = torch.tensor([node.attrs for node in nodes])

    def build_agent_input(self, data: dict, device: torch.device) -> Data:
        """
        Params:
          - `routes` contains, for each vehicle, the list of nodes (id) in the order it visits them.
        """
        routes = data["state"]
        edges = self.compute_edge_routes(routes)
        edge_attrs = self.compute_edge_attributes(routes)
        graph = Data(self.node_data, edges, edge_attr=edge_attrs)
        graph.validate()
        return graph.to(device.index, non_blocking=True)

    def is_compatible_with(self, other: Self) -> bool:
        if self.n_vehicles != other.n_vehicles:
            return False
        if len(self.nodes) != len(other.nodes):
            return False
        return self.nodes[0].n_dims == other.nodes[0].n_dims

    @cached_property
    def n_nodes(self) -> int:
        return len(self.nodes)

    @cached_property
    def n_node_features(self) -> int:
        return self.node_data.size(1)

    @property
    @abstractmethod
    def n_edge_features(self) -> int:
        """The number of features of each edge in the graph"""

    @staticmethod
    def compute_edge_routes(routes: list[list[int]]) -> torch.Tensor:
        """
        Compute the sources and the destinations of each edge in the graph.

        For instance, if a vehicle route is [1, 10, 15], the sources will be [1, 10, 15] and the corresponding destinations will be [10, 15, 1].
        """
        sources, destinations = [], []
        for vehicle_route in routes:
            vehicle_id = vehicle_route[0]
            sources.extend(vehicle_route)
            destinations.extend(vehicle_route[1:] + [vehicle_id])
        return torch.tensor([sources, destinations], dtype=torch.long)

    @abstractmethod
    def compute_edge_attributes(self, routes: list[list[int]]) -> torch.Tensor:
        """Compute the attributes of each edge, typically time and load information."""

    @staticmethod
    def compute_distance_matrix(locations: list[Coord]) -> list[list[float]]:
        n = len(locations)
        dist_matrix = [[0.0] * n for _ in range(n)]
        for i in range(n):
            coords1 = locations[i]
            for j in range(i + 1, n):
                coords2 = locations[j]
                dist = sum((c1 - c2) ** 2 for c1, c2 in zip(coords1, coords2)) ** 0.5
                dist_matrix[i][j] = dist
                dist_matrix[j][i] = dist
        return dist_matrix

    @staticmethod
    def normalize_coords(locations: list[Coord]):
        n_coords = len(locations[0])
        locations = locations + [(0.0,) * n_coords]
        locations = VRP.ensure_positive(locations)
        n_coords = len(locations[0])
        max_coord = max([max(loc[i] for loc in locations) for i in range(n_coords)])
        res = list[Coord]()
        for coord in locations:
            res.append(tuple(c / max_coord for c in coord))
        return res

    @staticmethod
    def ensure_positive(coords: list[Coord]):
        n_dims = len(coords[0])
        mins = [min(coord[i] for coord in coords) for i in range(n_dims)]
        return [(tuple(c - m for c, m in zip(coord, mins))) for coord in coords]

    @staticmethod
    def normalize_dist_matrix(arr: np.ndarray) -> list[list[float]]:
        max_value = np.max(arr)
        assert max_value > 0
        return (arr / max_value).tolist()
