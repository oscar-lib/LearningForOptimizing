from typing import overload, Optional
import orjson
from dataclasses import dataclass
from functools import cached_property


@dataclass
class ObjOverTime:
    timestamps: list[float]
    objectives: list[float]
    steps: list[int]

    @overload
    def at(self, *, step: int) -> float: ...

    @overload
    def at(self, *, t: float) -> float: ...

    def at(self, *, t: Optional[float] = None, step: Optional[int] = None) -> float:
        match (t, step):
            case (None, None):
                raise ValueError("Either t or step must be provided")
            case (float(), int()):
                raise ValueError("Only one of t or step must be provided")
            case (None, int(step)):
                target = step
                values = self.steps
            case (float(t), None) | (int(t), None):
                target = t
                values = self.timestamps
            case other:
                raise ValueError(f"Unexpected case: {other}")
        if target <= values[0]:
            return self.objectives[0]
        if target >= values[-1]:
            return self.objectives[-1]
        # Dichotomic search across steps or timestamps
        left, right = 1, len(values) - 1
        idx = (left + right) // 2
        while not (values[idx] > target and values[idx - 1] <= target):
            if target >= values[idx]:
                # If the target is greater than the value at idx,
                # then the lower bound is increased to the next index
                left = idx + 1
            else:
                # If the target is lower, idx could be the first index greater than target,
                # so we set the upper bound to idx rather than idx - 1.
                right = idx
            idx = (left + right) // 2
        return self.objectives[idx - 1]

    @staticmethod
    def parse(sol_over_time: str):
        # Format [(t:0.423-step:125-v:361.000)-(t:0.424-step:127-v:360.000)]
        sol_over_time = sol_over_time.replace("(t:", '{"t":')
        sol_over_time = sol_over_time.replace("-step:", ',"step":')
        # sol_over_time = sol_over_time.replace("-t:", ',"step":')
        sol_over_time = sol_over_time.replace("-v:", ',"v":')
        sol_over_time = sol_over_time.replace(")-", "},")
        sol_over_time = sol_over_time.replace(")]", "}]")
        data = orjson.loads(sol_over_time)
        if len(data) == 0:
            raise ValueError("No data points provided in solution over time")

        # flatten to two lists instead of a list of objects
        timestamps = [point["t"] for point in data]
        steps = [int(point["step"]) for point in data]
        objectives = [point["v"] for point in data]
        return ObjOverTime(timestamps, objectives, steps)

    @property
    def last(self):
        return self.objectives[-1]

    @cached_property
    def t_start(self) -> float:
        return self.timestamps[0]

    @property
    def best_obj_after(self):
        """Returns after how many seconds the best objective was found."""
        return self.timestamps[-1] - self.t_start
