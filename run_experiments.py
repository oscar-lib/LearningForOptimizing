import logging
import multiprocessing as mp
import os
import re
import subprocess
import time
from dataclasses import dataclass
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
        device: str = "auto",
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
            try:
                args_str = BEST_PARAMS[self.problem][self.bandit][self.reward]
            except KeyError:
                raise ValueError(f"No arguments provided and there is no BEST_PARAMS for {self.problem}, {self.bandit}, {self.reward}")
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
    output_filename: Optional[str]
    n_jobs: int
    n_repeats: int
    timeout: int
    seed: int
    problems: list[Literal["csp", "tsp", "pdptw"]]
    args: Optional[dict[str, Any]]

    def __init__(
        self,
        bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"],
        problems_file: Literal["csp", "tsp", "pdptw"] | str,
        reward: Literal["r1", "r2", "r3"],
        output_filename: Optional[str] = "auto",
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
        self.problems = self._load_problems()  # type: ignore
        if output_filename == "auto":
            output_filename = os.path.join("results", f"{datetime.now().isoformat().replace(':', '-')}-{self.problem}.csv")
        self.output_filename = output_filename
        self.n_jobs = n_jobs
        self.n_repeats = n_repeats
        self.timeout = timeout
        self.seed = seed
        self.args = args

    @property
    def problem(self):
        parts = self.problems[0].split("/")
        assert parts[0] == "examples"
        return parts[1]

    def _load_problems(self):
        with open(self.problems_file, "r") as f:
            return [line.strip() for line in f if line.strip()]

    def single_args(self):
        job_num = 0
        n_devices = torch.cuda.device_count()
        for seed in range(self.seed, self.seed + self.n_repeats):
            for problem in self.problems:
                # The first n_jobs runs are given a specific GPU
                if job_num < self.n_jobs:
                    device = f"cuda:{job_num % n_devices}"
                else:
                    device = "auto"
                yield SingleArgs(
                    bandit=self.bandit,
                    problem_path=problem,
                    reward=self.reward,
                    timeout=self.timeout,
                    seed=seed,
                    args=self.args,
                    device=device,
                )
                job_num += 1


@dataclass
class RunResult:
    metrics: dict
    bandit: Literal["epsilongreedy", "random", "ucb1", "dqn", "ppo"]
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

    @property
    def integral_primal_gap(self) -> float:
        return self.metrics["integralPrimalGap"]


def gather_results(stdout: bytes):
    output = stdout.decode("utf-8").strip()
    # Find all key=value pairs (keys can have underscores or hyphens)
    pairs = re.findall(r"([a-zA-Z_-][a-zA-Z0-9_-]*)\s*=\s*([^\n]+)", output)
    metrics = dict([(key.strip(), value) for key, value in pairs])
    metrics.pop("--communication", None)

    if "bestObj" not in metrics:
        raise ValueError(f"No objective found in output: {output}")
    metrics["bestObj"] = float(metrics["bestObj"])
    if "solOverTime" not in metrics:
        raise ValueError(f"No solution over time found in output: {output}")
    if "integralPrimalGap" not in metrics:
        raise ValueError(f"No integral primal gap found in output: {output}")
    metrics["integralPrimalGap"] = float(metrics["integralPrimalGap"])
    return metrics


def single_run(args: SingleArgs):
    cmd = f"{EXECUTABLE} {args.params}"
    logging.info(f"Running command: {cmd}")
    process = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    if process.returncode != 0:
        logging.error(f"Command failed with return code {process.returncode}")
        logging.error(f"Error output: {process.stderr.decode('utf-8')}")
        raise RuntimeError(f"Command failed: {cmd}")
    result_dict = gather_results(process.stdout)
    logging.info(f"{args.as_csv()},{result_dict}")
    return RunResult(
        metrics=result_dict,
        bandit=args.bandit,
        instance=args.problem_path,
        reward=args.reward,
        timeout=args.timeout,
        seed=args.seed,
    )


def multiple_runs(args: MultipleArgs):
    n_devices = torch.cuda.device_count()
    if n_devices == 0:
        logging.error("No GPU devices found for multiple runs. Exiting.")
        exit()
    results = list[RunResult]()
    results_file = None
    csv_columns = None
    if args.output_filename is not None:
        os.makedirs(os.path.dirname(args.output_filename), exist_ok=True)
        results_file = open(args.output_filename, "w")
    if args.n_jobs == 1:
        return [single_run(single_args) for single_args in args.single_args()]

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
                            if csv_columns is None:
                                csv_columns = result.get_columns()
                                results_file.write(",".join(csv_columns) + "\n")
                            results_file.write(result.as_csv(csv_columns) + "\n")
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
        logging.info(f"Results written to {args.output_filename}")
    return results


def main():
    multiple_runs(
        MultipleArgs(
            bandit="dqn",
            problems_file="csp",
            reward="r2",
            n_repeats=10,
            timeout=900,
            n_jobs=8,
            seed=0,
            output_filename="results/csp-r2-dqn-no-target.csv",
        )
    )


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
