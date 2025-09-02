import logging
import multiprocessing as mp
import os
import re
import subprocess
from dataclasses import dataclass
import time
from datetime import datetime
from typing import Any, Literal, Optional

import dotenv
import orjson
import torch

EXECUTABLE = "java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
with open("best_params.json", "rb") as f:
    BEST_PARAMS = orjson.loads(f.read())


@dataclass
class SingleArgs:
    bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"]
    problem_path: str
    reward: Literal["r1", "r2", "r3"]
    args: str
    timeout: int
    seed: int
    device: str

    def __init__(
        self,
        bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"],
        problem_path: str,
        reward: Literal["r1", "r2", "r3"],
        device: str,
        timeout: int = 300,
        seed: int = 0,
        args: Optional[dict[str, Any]] = None,
    ):
        self.bandit = bandit
        self.problem_path = problem_path
        self.reward = reward
        self.timeout = timeout
        self.seed = seed
        self.device = device
        if args is None:
            args_str = BEST_PARAMS[self.problem][self.bandit][self.reward]
        else:
            args_str = ""
            for key, value in args.items():
                if isinstance(value, bool):
                    if value:
                        args_str += f"--{key} "
                else:
                    args_str += f"--{key} {value} "
        self.args = args_str.strip()

    @property
    def problem(self):
        assert self.problem_path is not None, "Problem path must be provided"
        parts = self.problem_path.split("/")
        assert parts[0] == "examples"
        return parts[1]

    @property
    def params(self):
        return f"--problem {self.problem} --input {self.problem_path} --bandit {self.bandit} --reward {self.reward} --timeout {self.timeout} {self.args} --seed {self.seed} --device={self.device}"

    def as_csv(self):
        return f"{self.problem_path},{self.bandit},{self.reward},{self.timeout}"


@dataclass
class MultipleArgs:
    bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"]
    problems_file: str
    reward: Literal["r1", "r2", "r3"]
    output_file: Optional[str]
    n_jobs: int
    n_repeats: int
    timeout: int
    seed: int
    problems: list[str]
    args: Optional[dict[str, Any]]

    def __init__(
        self,
        bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"],
        problems_file: Literal["csp", "tsp", "pdptw"] | str,
        reward: Literal["r1", "r2", "r3"],
        output_file: str = "auto",
        n_jobs: int = 1,
        n_repeats: int = 20,
        timeout: int = 300,
        seed: int = 0,
        args: Optional[dict[str, Any]] = None,
    ):
        self.bandit = bandit
        if problems_file in ("csp", "tsp", "pdptw"):
            self.problems_file = os.path.join("examples", problems_file, "testingall.txt")
        else:
            self.problems_file = problems_file
        self.reward = reward
        if output_file == "auto":
            output_file = os.path.join("results", f"{datetime.now().isoformat().replace(':', '-')}.csv")
        self.output_file = output_file
        self.n_jobs = n_jobs
        self.n_repeats = n_repeats
        self.timeout = timeout
        self.seed = seed
        self.problems = self._load_problems()
        self.args = args

    def _load_problems(self):
        with open(self.problems_file, "r") as f:
            return [line.strip() for line in f if line.strip()]

    def single_args(self):
        device_num = 0
        device_count = torch.cuda.device_count()
        for seed in range(self.seed, self.seed + self.n_repeats):
            for problem in self.problems:
                yield SingleArgs(
                    bandit=self.bandit,
                    problem_path=problem,
                    reward=self.reward,
                    timeout=self.timeout,
                    seed=seed,
                    args=self.args,
                    device=f"cuda:{device_num % device_count}" if torch.cuda.is_available() else "cpu",
                )
                device_num += 1


@dataclass
class RunResult:
    sol_over_time: str
    objective: float
    integral_primal_gap: float
    bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"]
    instance: str
    reward: Literal["r1", "r2", "r3"]
    timeout: int
    seed: int

    CSV_HEADER = "instance,bandit,reward,timeout,seed,sol_over_time,objective,integral_primal_gap"

    def as_csv(self):
        fields = RunResult.CSV_HEADER.split(",")
        values = [getattr(self, field) for field in fields]
        return ",".join(map(str, values))


def gather_results(stdout: bytes):
    output = stdout.decode("utf-8").strip()
    objective_match = re.search(r"bestObj\s*=\s*([^\n]+)", output)
    if objective_match is None:
        raise ValueError(f"No objective found in output: {output}")
    objective = float(objective_match.group(1))

    sol_over_time_match = re.search(r"solOverTime\s*=\s*([^\n]+)", output)
    if sol_over_time_match is None:
        raise ValueError(f"No solution over time found in output: {output}")
    sol_over_time = sol_over_time_match.group(1)

    integral_primal_gap_match = re.search(r"integralPrimalGap\s*=\s*([^\n]+)", output)
    if integral_primal_gap_match is None:
        raise ValueError(f"No integral primal gap found in output: {output}")
    integral_primal_gap = float(integral_primal_gap_match.group(1))

    return sol_over_time, objective, integral_primal_gap


def single_run(args: SingleArgs):
    cmd = f"{EXECUTABLE} {args.params}"
    logging.info(f"Running command: {cmd}")
    process = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    if process.returncode != 0:
        logging.error(f"Command failed with return code {process.returncode}")
        logging.error(f"Error output: {process.stderr.decode('utf-8')}")
        raise RuntimeError(f"Command failed: {cmd}")
    sot, obj, ipg = gather_results(process.stdout)
    logging.info(f"{args.as_csv()},{sot},{obj},{ipg}")
    return RunResult(
        sol_over_time=sot,
        objective=obj,
        integral_primal_gap=ipg,
        bandit=args.bandit,
        instance=args.problem_path,
        reward=args.reward,
        timeout=args.timeout,
        seed=args.seed,
    )


def multiple_runs(args: MultipleArgs):
    results = list[RunResult]()
    results_file = None
    if args.output_file is not None:
        os.makedirs(os.path.dirname(args.output_file), exist_ok=True)
        results_file = open(args.output_file, "w")
        results_file.write(RunResult.CSV_HEADER + "\n")

    with mp.Pool(args.n_jobs) as pool:
        handles = [pool.apply_async(single_run, (single_args,)) for single_args in args.single_args()]
        # Collect the results as they become available
        dirty = True
        while len(handles) > 0:
            if dirty:
                logging.info(f"Waiting for {len(handles)} results...")
                dirty = False
            to_remove = []
            for handle in handles:
                if handle.ready():
                    try:
                        result = handle.get()
                        results.append(result)
                        to_remove.append(handle)
                        if results_file is not None:
                            results_file.write(result.as_csv() + "\n")
                            results_file.flush()
                    except Exception as e:
                        logging.error(f"Error processing result: {e}", exc_info=True)
                        to_remove.append(handle)
            for handle in to_remove:
                dirty = True
                handles.remove(handle)
            time.sleep(0.1)  # Avoid busy waiting
    if results_file is not None:
        results_file.close()
        logging.info(f"Results written to {args.output_file}")
    return results


def main():
    args = MultipleArgs("dqn", "tsp", "r3", n_repeats=1, timeout=5, n_jobs=2)
    multiple_runs(args)


if __name__ == "__main__":
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(process)d - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"{datetime.now().isoformat()}.log")],
    )
    try:
        main()
    except Exception as e:
        logging.error(f"An error occurred: {e}", exc_info=True)
    finally:
        logging.info("Runner finished execution.")
