from dataclasses import dataclass
import numpy as np
import json
import torch
from torch_geometric.data import Data
from icecream import ic


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
    is_depot: bool

    def get_data(self, t_max: int, vehicle_capacity: int, n_vehicles: int, delivery_ids: dict[int, int]):
        one_hot = [0] * len(delivery_ids)
        if not self.is_depot:
            delivery_id = delivery_ids[self.num - n_vehicles]
            one_hot[delivery_id] = 1
        return [
            self.x,
            self.y,
            self.earliest_arrival / t_max,
            self.latest_arrival / t_max,
            self.duration / t_max,
            self.delta_load / vehicle_capacity,
        ] + one_hot

    @property
    def is_source(self):
        return self.delta_load > 0

    @property
    def is_destination(self):
        return self.delta_load < 0

    def distance(self, other: "Node") -> float:
        return ((self.x - other.x) ** 2 + (self.y - other.y) ** 2) ** 0.5


@dataclass
class Edge:
    current_route_duration: float
    """Duration of the route from depot to the endpoint of the edge."""
    current_load: int
    """Current load of the vehicle (between src and dst)."""

    @property
    def as_tensor(self):
        pass


@dataclass
class Problem:
    n_vehicles: int
    vehicle_capacity: int
    nodes: list[Node]
    t_max: int
    n_actions: int
    node_data: torch.Tensor
    n_node_features: int
    n_nodes: int

    def __init__(self, n_vehicles: int, vehicle_capacity: int, nodes: list[Node], n_actions: int, delivery_ids: dict[int, int]):
        self.n_vehicles = n_vehicles
        self.vehicle_capacity = vehicle_capacity
        self.nodes = nodes
        self.n_actions = n_actions
        self.t_max = self._compute_t_max(nodes)
        self.node_data = torch.tensor([node.get_data(self.t_max, self.vehicle_capacity, n_vehicles, delivery_ids) for node in self.nodes])
        self.n_node_features = self.node_data.size(1)
        self.n_nodes = len(self.nodes)

    @classmethod
    def parse(cls, bdata: bytes) -> "Problem":
        """
        Parse the static data of a PDPTW problem in a JSON format.
        """
        data = json.loads(bdata)
        n_vehicles = len(data["vehicles"])
        vehicle_capacity = data["vehicles"][0]["capacity"]
        n_actions = data["nActions"]
        delivery_ids = dict[int, int]()  # map each node to the coreesponding delivery ID
        for i, delivery in enumerate(data["demands"]):
            source = delivery["fromNodeId"]
            destination = delivery["toNodeId"]
            delivery_ids[source] = i
            delivery_ids[destination] = i
        nodes = []
        for node in data["nodes"]:
            # Since each vehicle has its own depot with the id of the vehicle, we need to add the number of vehicles to the node id
            node_id = node["nodeId"] + n_vehicles
            nodes.append(
                Node(
                    num=node_id,
                    x=node["positionXY"][0],
                    y=node["positionXY"][1],
                    earliest_arrival=node["earliestArrival"],
                    latest_arrival=node["latestArrival"],
                    duration=node["duration"],
                    delta_load=node["quantity"],
                    is_depot=False,
                )
            )
        t_max = cls._compute_t_max(nodes)
        # Add the depot nodes at the front of the node list
        # TODO: déplacer les noeuds pour que le dépot soit d'office en 0, 0
        depots = [
            Node(num=i, x=0.0, y=0.0, earliest_arrival=0, latest_arrival=t_max, duration=0, delta_load=0, is_depot=True)
            for i in range(n_vehicles)
        ]

        return Problem(
            n_vehicles=n_vehicles,
            vehicle_capacity=vehicle_capacity,
            nodes=depots + nodes,
            n_actions=n_actions,
            delivery_ids=delivery_ids,
        )

    def build_agent_input(self, routes: list[list[int]]) -> Data:
        """
        Params:
          - `routes` contains, for each vehicle, the list of nodes (id) in the order it visits them.
        """
        edges = self._compute_edges(routes)
        edge_attrs = self._compute_edge_attributes(routes)
        nodes = self.node_data
        graph = Data(nodes, edges, edge_attr=edge_attrs)
        graph.validate()
        return graph

    @staticmethod
    def _compute_edges(routes: list[list[int]]) -> torch.Tensor:
        edges = [[], []]  # sources, destinations
        for vehicle_route in routes:
            vehicle_id = vehicle_route[0]
            edges[0].extend(vehicle_route)
            edges[1].extend(vehicle_route[1:] + [vehicle_id])
        return torch.tensor(edges, dtype=torch.long)

    def _compute_edge_attributes(self, routes: list[list[int]]) -> torch.Tensor:
        """
        Compute the edge attributes, i.e.:
            - the time at which src was left
            - the time at which dst was reached
            - the load of the vehicle while traveling from src to dst
        """
        attributes = []
        for route in routes:
            current_load = 0
            current_time = 0
            src = depot = self.nodes[route[0]]
            for node_num in route[1:]:
                edge_attributes = [current_time / self.t_max]  # time we left src
                dst = self.nodes[node_num]
                current_time += dst.distance(src)
                if current_time < dst.earliest_arrival:
                    current_time = dst.earliest_arrival
                # Time we reach dst and load during the travel between src and dst
                edge_attributes.extend([current_time / self.t_max, current_load / self.vehicle_capacity])
                attributes.append(edge_attributes)
                current_load += dst.delta_load
                src = dst
            # Add the last edge (from the last node of the route to the depot)
            edge_attributes = [current_time / self.t_max]
            current_time += src.distance(depot)
            edge_attributes.extend([current_time / self.t_max, current_load / self.vehicle_capacity])
            attributes.append(edge_attributes)
        ic(attributes)
        return torch.tensor(attributes)

    @staticmethod
    def _compute_t_max(nodes: list[Node]) -> int:
        latest_arrival_index = np.argmax([node.latest_arrival for node in nodes])
        latest_node = nodes[latest_arrival_index]
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
