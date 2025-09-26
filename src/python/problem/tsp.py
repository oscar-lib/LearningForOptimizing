from typing import Self

import numpy as np
import orjson
import torch

from .vrp import VRP, VRPNode


class TSP(VRP[VRPNode]):
    def __init__(self, n_actions: int, nodes: list[VRPNode]):
        super().__init__(1, nodes, n_actions)

    @property
    def n_edge_features(self):
        return 2

    @staticmethod
    def parse(bdata: bytes):
        data: dict = orjson.loads(bdata)
        problem = data["problem"]
        coords = problem["cityCoords"]
        distance_matrix = VRP.normalize_dist_matrix(np.array(problem["distances"]))
        n_cities = len(distance_matrix)
        coords = VRP.normalize_coords(coords)
        nodes = list[VRPNode]()
        for i in range(n_cities):
            if len(coords) == 0:
                coord = ()
            else:
                coord = coords[i]
            nodes.append(VRPNode(i, coord, distance_matrix[i]))
        return TSP(data["nActions"], nodes)

    def is_compatible_with(self, other: Self) -> bool:
        if self.n_nodes != other.n_nodes:
            return False
        if self.n_actions != other.n_actions:
            return False
        return self.n_node_features == other.n_node_features

    def compute_edge_attributes(self, routes: list[list[int]]):
        """
        Compute the edge attributes, i.e.:
            - the time at which src was left
            - the time at which dst was reached

        **IMPORTANT**: routes assume a 0-based indexing for the nodes.
        """
        attributes = []
        for route in routes:
            current_time = 0
            src = depot = self.nodes[route[0]]
            for node_num in route[1:]:
                start = current_time
                dst = self.nodes[node_num]
                current_time += dst.distance_vector[src.index]
                attributes.append([start, current_time])
                src = dst
            # Add the last edge (from the last node of the route to the depot)
            attributes.append([current_time, current_time + src.distance_vector[depot.index]])
        return torch.tensor(attributes)
