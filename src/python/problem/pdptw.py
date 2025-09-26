from typing import Optional
import orjson
import torch
import logging
from torch_geometric.data import Data

from .vrp import VRP, VRPNode


class PDPTWNode(VRPNode):
    earliest_arrival: float
    latest_arrival: float
    duration: float
    """The time it takes to visit this node (pickup or delivery)."""
    delta_load: float
    """How much the vehicle load changes when visiting this node (negative for delivery, positive for pickups)."""
    delivery: list[float]
    """One-hot encoding of the delivery ID associated with this node. All zeros for depots."""

    def __init__(
        self,
        index: int,
        coords: tuple[float, ...],
        distance_vector: list[float],
        earliest_arrival: float,
        latest_arrival: float,
        duration: float,
        delta_load: float,
        one_hot_delivery_id: list[float],
    ):
        super().__init__(index, coords, distance_vector)
        self.earliest_arrival = earliest_arrival
        self.latest_arrival = latest_arrival
        self.duration = duration
        self.delta_load = delta_load
        self.delivery = one_hot_delivery_id

    @property
    def attrs(self):
        return [*super().attrs, self.latest_arrival, self.earliest_arrival, self.duration, self.delta_load, *self.delivery]

    @property
    def is_source(self):
        return self.delta_load > 0

    @property
    def is_destination(self):
        return self.delta_load < 0


class PDPTW(VRP[PDPTWNode]):
    vehicle_capacity: int
    t_max: int
    n_actions: int

    def __init__(self, n_vehicles: int, vehicle_capacity: int, nodes: list[PDPTWNode], n_actions: int):
        super().__init__(n_vehicles, nodes, n_actions)
        self.vehicle_capacity = vehicle_capacity

    @classmethod
    def parse(cls, bdata: bytes) -> "PDPTW":
        """
        Parse the static data of a PDPTW problem in a JSON format.
        """
        data: dict = orjson.loads(bdata)
        problem = data["problem"]["liLimProblem"]
        n_actions: int = data["nActions"]
        vehicle_capacities = [vehicle["capacity"] for vehicle in problem["vehicles"]]
        assert all(capacity == vehicle_capacities[0] for capacity in vehicle_capacities), "All vehicles must have the same capacity"
        capacity = vehicle_capacities[0]
        t_max: int = max(node["latestArrival"] + node["duration"] for node in problem["nodes"])
        delivery_coords = [node["positionXY"] for node in problem["nodes"]]
        depots_coords = [vehicle["depot"]["positionXY"] for vehicle in problem["vehicles"]]
        if all(depots_coords[0] == depot for depot in depots_coords):
            logging.info("All depots are the same, keeping only one for the distance matrix computation.")
            depots_coords = [depots_coords[0]]  # If all depots are the same, keep only one for the distance matrix computation

        coords = VRP.normalize_coords(delivery_coords + depots_coords)
        dist_matrix = VRP.compute_distance_matrix(coords)
        deliveries = dict[int, int]()  # map each node to the corresponding delivery ID
        for delivery, delivery in enumerate(problem["demands"]):
            deliveries[delivery["fromNodeId"] - 1] = delivery
            deliveries[delivery["toNodeId"] - 1] = delivery

        nodes = list[PDPTWNode]()
        # Add actual nodes
        for node in problem["nodes"]:
            node_id = node["nodeId"] - 1
            delivery = [0.0] * len(deliveries)
            delivery[node_id] = 1.0
            node = PDPTWNode(
                node_id,
                coords[node_id],
                dist_matrix[node_id],
                node["earliestArrival"] / t_max,
                node["latestArrival"] / t_max,
                node["duration"] / t_max,
                node["quantity"] / capacity,
                delivery,
            )
            nodes.append(node)
        # Add depots
        for num_depot in range(len(depots_coords)):
            index = num_depot + len(delivery_coords)
            node = PDPTWNode(index, coords[index], dist_matrix[index], 0, 0, 0, 0, [0.0] * len(deliveries))
            nodes.append(node)
        return PDPTW(len(problem["vehicles"]), capacity, nodes, n_actions)

    def build_agent_input(self, data: dict, device: torch.device) -> Data:
        data["state"] = [[n - 1 for n in route] for route in data["state"]]  # Convert to zero-based indexing
        return super().build_agent_input(data, device)

    def compute_edge_attributes(self, routes: list[list[int]]):
        """
        Compute the edge attributes, i.e.:
            - the time at which src was left
            - the time at which dst was reached
            - the load of the vehicle while traveling from src to dst

        **IMPORTANT**: routes assume a 0-based indexing for the nodes.
        """
        attributes = []
        for route in routes:
            current_load = 0
            current_time = 0
            src = depot = self.nodes[route[0]]
            for node_num in route[1:]:
                start = current_time
                dst = self.nodes[node_num]
                current_time += dst.distance_vector[src.index]
                if current_time < dst.earliest_arrival:
                    current_time = dst.earliest_arrival
                attributes.append([start, current_time, current_load])
                current_load += dst.delta_load
                src = dst
            # Add the last edge (from the last node of the route to the depot)
            attributes.append([current_time, current_time + src.distance_vector[depot.index], current_load])
        return torch.tensor(attributes)

    @property
    def n_edge_features(self):
        # - time of leaving src (normalized)
        # - vehicle load from src to dst (normalized)
        # - time of reaching dst (normalized)
        return 3

    def __eq__(self, other) -> bool:
        if not isinstance(other, PDPTW):
            return False
        if self.n_vehicles != other.n_vehicles:
            return False
        if self.vehicle_capacity != other.vehicle_capacity:
            return False
        if self.n_actions != other.n_actions:
            return False
        if self.n_nodes != other.n_nodes:
            return False
        return len(self.nodes) == len(other.nodes)

    def is_compatible_with(self, other) -> bool:
        if not isinstance(other, PDPTW):
            return False
        return self.n_vehicles == other.n_vehicles and self.n_actions == other.n_actions and self.n_nodes == other.n_nodes
