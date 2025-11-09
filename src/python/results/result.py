from functools import cached_property, lru_cache
from typing import Any, Literal, Optional, overload

import polars as pl

from .bks import BKS, primal_gap
from .obj_over_time import ObjOverTime

UNROUTED_NODE_PENALTY = 1000000000
Bandit = Literal["epsilongreedy", "random", "ucb", "dqn", "ppo", "dqn-no-target", "dqn-no-target-300", "bestslopefirst"]


class Result:
    metrics: dict
    bandit: Bandit
    instance: str
    reward: Literal["r1", "r2", "r3"]
    timeout: int
    seed: int

    def __init__(
        self,
        bandit: Bandit,
        instance: str,
        reward: Literal["r1", "r2", "r3"],
        timeout: int,
        seed: int,
        **metrics: Any,
    ):
        self.bandit = bandit
        self.instance = instance
        self.reward = reward
        self.timeout = timeout
        self.seed = seed
        self.metrics = metrics

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

    @overload
    def objective(self, *, t: float) -> float:
        """Objective value at time `t`."""

    @overload
    def objective(self, *, step: int) -> float:
        """Objective value at step number `step`."""

    @lru_cache
    def objective(self, **kwargs) -> float:
        return self.oot.at(**kwargs)

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

    @overload
    def is_feasible(self, *, t: float) -> bool: ...
    @overload
    def is_feasible(self, *, step: int) -> bool: ...

    @lru_cache
    def is_feasible(self, **kwargs) -> bool:
        """Whether the solution is feasible or not at timestamp t."""
        match self.problem:
            case "csp":
                return self.objective(**kwargs) == 0
            case "tsp" | "pdptw":
                return self.objective(**kwargs) < UNROUTED_NODE_PENALTY
            case _:
                raise ValueError(f"Unknown problem type: {self.problem}")

    @overload
    def primal_gap(self, *, t: float) -> float:
        pass

    @overload
    def primal_gap(self, *, step: int) -> float:
        pass

    def primal_gap(self, *, t: Optional[float] = None, step: Optional[int] = None):
        match (t, step):
            case (None, None):
                raise ValueError("Either t or step must be provided")
            case (float(), int()):
                raise ValueError("Only one of t or step must be provided")
            case (None, int(step)) | (None, float(step)):
                assert step >= 0
                kwargs = {"step": step}
            case (float(t), None) | (int(t), None):
                assert t >= 0
                kwargs = {"t": t}
        return primal_gap(self.objective(**kwargs), self.bks, self.is_feasible(**kwargs))

    def is_optimal(self):
        return self.oot.last == self.bks

    @property
    def t_max(self) -> float:
        return self.oot.timestamps[-1]

    @property
    def step_max(self) -> int:
        return self.oot.steps[-1]

    def feasible_array(self, by: Literal["time", "step"], tmax: Optional[int] = None):
        objs = self.objective_array(by, tmax=tmax)
        match self.problem:
            case "csp":
                return [obj == 0 for obj in objs]
            case "tsp" | "pdptw":
                return [obj < UNROUTED_NODE_PENALTY for obj in objs]
        raise ValueError(f"Unknown problem type: {self.problem}")

    def objective_array(self, by: Literal["time", "step"], tmax: Optional[int] = None):
        if by == "step":
            xs = self.oot.steps
        elif by == "time":
            xs = self.oot.timestamps
        else:
            raise ValueError(f"Unknown by value: {by}")
        if tmax is None:
            tmax = int(xs[-1]) + 1
        objectives = list[float]()
        index = 1
        for t in range(tmax):
            # Reach the first index where xs[index] > t
            while index < len(xs) and xs[index] <= t:
                index += 1
            # The previous objective is the value at time/step t
            objectives.append(self.oot.objectives[index - 1])
        return objectives

    def primal_gap_array(self, by: Literal["time", "step"], tmax: Optional[int] = None):
        objs = self.objective_array(by, tmax=tmax)
        feas = self.feasible_array(by, tmax=tmax)
        return [primal_gap(o, self.bks, f) for o, f in zip(objs, feas)]
