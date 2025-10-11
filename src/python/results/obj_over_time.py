import orjson
from dataclasses import dataclass
from functools import cached_property


@dataclass
class ObjOverTime:
    timestamps: list[float]
    objectives: list[float]
    steps: list[int]

    def at(self, t: float):
        assert t >= 0
        # Dichotomic search across timestamps
        left, right = 0, len(self.timestamps) - 1
        if t > self.timestamps[-1]:
            return self.objectives[-1]
        while left < right:
            idx = (left + right) // 2
            if self.timestamps[idx] == t:
                return self.objectives[idx]
            if self.timestamps[idx] < t:
                left = idx + 1
            else:
                right = idx - 1
        return self.objectives[idx]

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
