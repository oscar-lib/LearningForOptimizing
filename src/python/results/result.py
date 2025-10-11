from dataclasses import dataclass
from functools import cached_property
from typing import Literal

import polars as pl

from .bks import BKS
from .obj_over_time import ObjOverTime

UNROUTED_NODE_PENALTY = 1000000000


@dataclass
class Result:
    metrics: dict
    bandit: Literal["epsilongreedy", "random", "ucb", "dqn", "ppo", "dqn-no-target", "dqn-no-target-300"]
    instance: str
    reward: Literal["r1", "r2", "r3"]
    timeout: int
    seed: int

    def get_columns(self):
        return [
            "bandit",
            "instance",
            "reward",
            "timeout",
            "seed",
        ] + list(self.metrics.keys())

    def as_csv(self, columns: list[str]):
        values = []
        for col in columns:
            try:
                values.append(getattr(self, col))
            except AttributeError:
                values.append(self.metrics.get(col, ""))
        return ",".join(map(str, values))

    def objective(self, t: float) -> float:
        """Objective value at time t."""
        return self.oot.at(t)

    @property
    def integral_primal_gap(self) -> float:
        return self.metrics["integralPrimalGap"]

    @property
    def best_obj(self) -> float:
        return self.metrics["bestObj"]

    @cached_property
    def oot(self):
        return ObjOverTime.parse(self.metrics["solOverTime"])

    @cached_property
    def problem(self):
        parts = self.instance.split("/")
        assert parts[0] == "examples"
        problem = parts[1]
        assert problem in ("csp", "tsp", "pdptw")
        return problem

    @cached_property
    def bks(self) -> float:
        row = BKS[self.problem].filter(pl.col("instance") == self.instance).select("bks")
        if row.is_empty():
            return 0.0
        return row.item()

    @property
    def n_secs_to_best_obj(self) -> float:
        return self.oot.best_obj_after

    def is_feasible(self, t: float) -> bool:
        """Whether the solution is feasible or not at timestamp t."""
        match self.problem:
            case "csp":
                return self.objective(t) == 0
            case "tsp" | "pdptw":
                return self.objective(t) < UNROUTED_NODE_PENALTY
            case _:
                raise ValueError(f"Unknown problem type: {self.problem}")

    def primal_gap(self, t: float):
        assert t >= 0.0
        if not self.is_feasible(t):
            return 1.0
        obj = self.objective(t)
        if obj == self.bks:
            return 0.0
        return abs(self.bks - obj) / max(abs(self.bks), abs(obj))

    def is_optimal(self):
        return self.oot.last == self.bks
