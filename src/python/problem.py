from dataclasses import dataclass
import numpy as np
import json
import torch
from torch_geometric.data import Data


@dataclass
class Node:
    num: int
    x: float
    y: float
    earliest_arrival: int
    latest_arrival: int
    duration: int
    """The time it takes to visit this node (pickup or delivery)."""
    delta_load: int
    """How much the vehicle load changes when visiting this node (negative for delivery, positive for pickups)."""

    def get_data(self, t_max: int, vehicle_capacity: int):
        return [
            self.x,
            self.y,
            self.earliest_arrival / t_max,
            self.latest_arrival / t_max,
            self.duration / t_max,
            self.delta_load / vehicle_capacity,
        ]


@dataclass
class Delivery:
    src: int
    dst: int


@dataclass
class Problem:
    n_vehicles: int
    vehicle_capacity: int
    nodes: list[Node]
    t_max: int
    n_actions: int
    node_data: torch.Tensor

    def __init__(self, n_vehicles: int, vehicle_capacity: int, nodes: list[Node], n_actions: int):
        self.n_vehicles = n_vehicles
        self.vehicle_capacity = vehicle_capacity
        self.nodes = nodes
        self.n_actions = n_actions
        self.t_max = self._compute_t_max()
        self.node_data = torch.tensor([node.get_data(self.t_max, self.vehicle_capacity) for node in self.nodes])

    @classmethod
    def parse(cls, bdata: bytes) -> "Problem":
        """
        Parse the static data of a PDPTW problem in a JSON format.
        """
        data = json.loads(bdata)
        n_vehicles = len(data["vehicles"])
        vehicle_capacity = data["vehicles"][0]["capacity"]
        nodes = [
            Node(
                num=node["nodeId"],
                x=node["positionXY"][0],
                y=node["positionXY"][1],
                earliest_arrival=node["earliestArrival"],
                latest_arrival=node["latestArrival"],
                duration=node["duration"],
                delta_load=node["quantity"],
            )
            for node in data["nodes"]
        ]
        return Problem(n_vehicles, vehicle_capacity, nodes, 5)

    @property
    def n_nodes(self):
        return len(self.nodes)

    @property
    def n_node_features(self):
        return self.node_data.size(1)

    def edge_data(self, routes: list[list[int]]):
        pass

    def _compute_t_max(self) -> int:
        latest_arrival_index = np.argmax([node.latest_arrival for node in self.nodes])
        latest_node = self.nodes[latest_arrival_index]
        latest_arrival = latest_node.latest_arrival
        duration = latest_node.duration
        time_to_depot = round(latest_node.x**2 + latest_node.y**2) ** 0.5
        return latest_arrival + duration + time_to_depot

    @property
    def n_edge_features(self):
        # - current time (normalized)
        # - current vehicle load (normalized)
        # - current time (normalized)
        return 3
