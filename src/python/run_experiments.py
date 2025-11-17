from io import TextIOWrapper
import logging
import multiprocessing as mp
from multiprocessing.pool import AsyncResult
import os
import re
import subprocess
import sys
import threading
import time
import random
from dataclasses import dataclass, astuple
from datetime import datetime
from typing import Any, Literal, Optional
from results import Result, Bandit
import typed_argparse as tap

import dotenv
import orjson
import torch

GPUS = list(range(torch.cuda.device_count()))
# GPUS.remove(4)


EXECUTABLE = "java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
with open("best_params.json", "rb") as f:
    BEST_PARAMS = orjson.loads(f.read())


def dict2arg(d: dict[str, Any]) -> str:
    args_str = ""
    for key, value in d.items():
        if isinstance(value, bool):
            if value:
                args_str += f"--{key} "
        else:
            args_str += f"--{key} {value} "
    return args_str.strip()


@dataclass
class SingleArgs:
    bandit: Bandit
    problem_path: str
    reward: Literal["r1", "r2", "r3"]
    args: str
    timeout: int
    seed: int
    device: str
    training: bool
    logdir: Optional[str] = None

    def __init__(
        self,
        bandit: Bandit,
        problem_path: str,
        reward: Literal["r1", "r2", "r3"],
        device: str = "auto",
        timeout: int = 300,
        seed: int = 0,
        training: bool = True,
        args: dict[str, Any] | str | None = None,
        logdir: Optional[str] = None,
    ):
        self.bandit = bandit
        self.problem_path = problem_path
        self.reward = reward
        self.timeout = timeout
        self.seed = seed
        self.device = device
        self.logdir = logdir
        self.training = training
        match args:
            case None:
                try:
                    args_str = BEST_PARAMS[self.problem][self.bandit][self.reward]
                except KeyError:
                    raise KeyError(f"No arguments provided and there is no BEST_PARAMS for {self.problem}, {self.bandit}, {self.reward}")
            case str():
                args_str = args
            case dict():
                args_str = dict2arg(args)
            case other:
                raise ValueError(f"Invalid type for args: {type(other)}")
        self.args = args_str

    @property
    def problem(self):
        parts = self.problem_path.split("/")
        assert parts[0] == "examples"
        return parts[1]

    @property
    def params(self):
        params = "--bandit "
        if self.bandit in ("dqn-no-target", "dqn-no-target-300"):
            params += "dqn --noTarget"
        else:
            params += f"{self.bandit}"
        params += f" --problem {self.problem} --input {self.problem_path} --reward {self.reward} --timeout {self.timeout} {self.args} --seed {self.seed} --device={self.device}"
        if not self.training:
            params += " --noTrain"
        if self.logdir is not None:
            params += f" --logdir {self.logdir}"
        return params

    def as_csv(self):
        return f"{self.problem_path},{self.bandit},{self.reward},{self.timeout}"

    def __hash__(self):
        return hash(astuple(self))


@dataclass
class MultipleArgs:
    bandit: Bandit
    problems_file: str
    reward: Literal["r1", "r2", "r3"]
    n_jobs: int
    n_repeats: int
    timeout: int
    seed: int
    training: bool
    instance_filenames: list[str]
    args: dict[str, Any] | str | None
    _require_gpu: bool
    logdir: str
    reuse_gpu: bool

    def __init__(
        self,
        bandit: Bandit,
        problems_file: Literal["csp", "tsp", "pdptw"] | str,
        reward: Literal["r1", "r2", "r3"],
        logdir: Optional[str] = None,
        n_jobs: int = 1,
        n_repeats: int = 20,
        timeout: int = 300,
        seed: int = 0,
        training: bool = True,
        args: dict[str, Any] | str | None = None,
        require_gpu: bool = True,
        reuse_gpu: bool = False,
    ):
        logdir_is_none = logdir is None
        if logdir is None or len(logdir) == 0:
            logdir = os.path.join("logs", datetime.now().isoformat().replace(":", "-"))
        elif not logdir.startswith("logs"):
            logdir = os.path.join("logs", logdir)
        os.makedirs(logdir, exist_ok=not logdir_is_none)
        logging.basicConfig(
            level=os.getenv("LOG_LEVEL", "INFO").upper(),
            format="%(asctime)s - %(process)d - %(levelname)s - %(message)s",
            handlers=[logging.StreamHandler(), logging.FileHandler(os.path.join(logdir, "output.log"))],
        )
        self.logdir = logdir
        self.bandit = bandit
        self.training = training
        if problems_file in ("csp", "tsp", "pdptw"):
            self.problems_file = os.path.join("examples", problems_file, "testingall.txt")
        else:
            self.problems_file = problems_file
        self.reward = reward
        self.instance_filenames = self._load_problem_instances()
        self.n_jobs = n_jobs
        self.n_repeats = n_repeats
        self.timeout = timeout
        self.seed = seed
        self.args = args
        self._require_gpu = require_gpu
        self.reuse_gpu = reuse_gpu
        self._write_config()

    def _write_config(self):
        IGNORED_KEYS = ("require_gpu", "n_repeats", "seed", "n_jobs", "reuse_gpu")
        config_path = os.path.join(self.logdir, "config.json")
        self_config_str = orjson.dumps(self)
        self_config: dict = orjson.loads(self_config_str)
        if os.path.exists(config_path):
            with open(config_path, "rb") as f:
                existing = orjson.loads(f.read())
            for key, value in self_config.items():
                if key in IGNORED_KEYS:
                    continue
                if key not in existing:
                    logging.error(f"Key {key} not in existing config")
                    raise ValueError(f"Existing config at {config_path} does not match the current configuration. Key {key} is missing.")
                if existing[key] != value:
                    logging.error(f"Value for key {key} differs: {existing[key]} != {value}")
                    raise ValueError(
                        f"Existing config at {config_path} does not match the current configuration. Key {key} differs ({existing[key]} != {value})."
                    )
        with open(config_path, "wb") as f:
            f.write(orjson.dumps(self, option=orjson.OPT_INDENT_2))

    @property
    def problem(self):
        parts = self.instance_filenames[0].split("/")
        assert parts[0] == "examples"
        return parts[1]

    def _load_problem_instances(self):
        with open(self.problems_file, "r") as f:
            return [line.strip() for line in f if line.strip()]

    def single_args(self):
        job_num = 0
        logdirs = dict[str, str]()
        for seed in range(self.seed, self.seed + self.n_repeats):
            instances = self.instance_filenames.copy()
            random.shuffle(instances)
            for instance in instances:
                if instance not in logdirs:
                    instance_name, *_ = os.path.basename(instance).split(".")
                    logdirs[instance] = os.path.join(self.logdir, f"{self.problem}-{instance_name}")

                # The first n_jobs runs are given a specific GPU
                if self._require_gpu:
                    n_devices = torch.cuda.device_count()
                    if n_devices == 0:
                        raise RuntimeError("No GPU devices available but require_gpu is set to True")
                    if job_num < self.n_jobs:
                        device = f"cuda:{GPUS[job_num % len(GPUS)]}"
                    else:
                        device = "auto-gpu"
                else:
                    device = "auto"
                yield SingleArgs(
                    bandit=self.bandit,
                    problem_path=instance,
                    reward=self.reward,
                    timeout=self.timeout,
                    seed=seed,
                    args=self.args,
                    device=device,
                    training=self.training,
                    logdir=logdirs[instance],
                )
                job_num += 1


def gather_results(stdout: bytes):
    output = stdout.decode("utf-8").strip()
    # Find all key=value pairs (keys can have underscores or hyphens)
    pairs = re.findall(r"([a-zA-Z_-][a-zA-Z0-9_-]*)\s*=\s*([^\n]+)", output)
    metrics = dict([(key.strip(), value) for key, value in pairs])
    metrics.pop("--communication", None)
    metrics.pop("-a", None)

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
    return Result(
        bandit=args.bandit,
        instance=args.problem_path,
        reward=args.reward,
        timeout=args.timeout,
        seed=args.seed,
        device=args.device,
        **result_dict,
    )


def perform_runs(
    single_args: list[SingleArgs],
    pool_size: int,
    results_file: TextIOWrapper,
    reuse_gpu: bool,
    csv_columns: Optional[list[str]] = None,
):
    queue = single_args.copy()
    del single_args
    success = list[Result]()
    running = dict[SingleArgs, AsyncResult[Result]]()
    failure = list[SingleArgs]()
    total_n_runs = len(queue)
    with mp.Pool(pool_size) as pool:
        start = datetime.now()
        for _ in range(pool_size):
            sa = queue.pop(0)
            running[sa] = pool.apply_async(single_run, (sa,))
        logging.info(f"Started {len(running)}/{total_n_runs} runs.")
        while len(running) > 0 or len(queue) > 0:
            to_remove = list[SingleArgs]()
            new_runs = dict[SingleArgs, AsyncResult[Result]]()
            for args, handle in running.items():
                if handle.ready():
                    to_remove.append(args)
                    try:
                        result = handle.get()
                        success.append(result)
                        if csv_columns is None:
                            csv_columns = result.get_columns()
                            results_file.write(",".join(csv_columns) + "\n")
                        results_file.write(result.as_csv(csv_columns) + "\n")
                        results_file.flush()
                    except Exception as e:
                        logging.error(f"Error processing result: {e}", exc_info=True)
                        failure.append(args)
                    if len(queue) > 0:
                        next_args = queue.pop(0)
                        if reuse_gpu:
                            next_args.device = args.device
                        new_runs[next_args] = pool.apply_async(single_run, (next_args,))
            for i in to_remove:
                running.pop(i)
            running = running | new_runs
            if len(to_remove) > 0:
                n_finished = len(success) + len(failure)
                n_remaining = total_n_runs - n_finished
                avg_time = (datetime.now() - start) / n_finished
                remaining = n_remaining * avg_time
                logging.info(f"[{len(success)} success|{len(failure)} failure|{n_remaining} remaining] ETA: {remaining}")
            time.sleep(0.2)  # Avoid busy waiting
    return success, failure


def multiple_runs(multi_args: MultipleArgs):
    if multi_args._require_gpu and torch.cuda.device_count() == 0:
        logging.error("No GPU devices found for multiple runs. Exiting.")
        exit()
    csv_columns = None
    os.makedirs(multi_args.logdir, exist_ok=True)
    results_filename = os.path.join(multi_args.logdir, "results.csv")
    if os.path.exists(results_filename):
        with open(results_filename, "r") as f:
            first_line = f.readline().strip()
            if len(first_line.strip()) == 0:
                mode = "w"
            else:
                mode = "a"
                assert first_line.startswith("bandit,instance,reward,timeout,seed")  # Basic check for column names
                csv_columns = first_line.split(",")
    else:
        mode = "w"
    single_args = list(multi_args.single_args())
    with open(results_filename, mode) as results_file:
        successes, failures = perform_runs(single_args, multi_args.n_jobs, results_file, multi_args.reuse_gpu, csv_columns)
        if len(failures) > 0:
            logging.warning(f"{len(failures)} runs failed. Retrying failed {len(failures)} runs...")
            s, failures = perform_runs(failures, len(GPUS), results_file, multi_args.reuse_gpu, csv_columns)
            successes.extend(s)
            if len(failures) > 0:
                logging.error(f"{len(failures)} runs failed again after retrying.")
    return successes, failures


def ask_recompile_with_countdown() -> bool:
    # Check if input is available
    if not sys.stdin.isatty():
        print("No interactive input available, recompiling.")
        return True

    def get_input(result):
        try:
            result.append(input().strip().lower())
        except OSError:  # Happens when input is not available, e.g. with nohup
            pass

    result = []
    input_thread = threading.Thread(target=get_input, args=(result,))
    input_thread.daemon = True
    input_thread.start()

    for i in range(30, 0, -1):
        sys.stdout.write(f"\r[{i / 10:.1f}s]\tDo you want to recompile? (y/n) ")
        sys.stdout.flush()
        input_thread.join(timeout=0.1)
        if not input_thread.is_alive():
            break
    print()  # Move to next line after countdown
    if len(result) == 0:
        print("No input within 3 seconds received, assuming 'yes'.")
    return len(result) == 0 or result[0] not in ("n", "")


class Args(tap.TypedArgs):
    no_compile: bool = tap.arg("--no-compile", help="Skip compilation step", default=False)

    @property
    def compile(self):
        return not self.no_compile


def main(args: Args):
    if args.compile and ask_recompile_with_countdown():
        subprocess.run("sbt assembly", shell=True, check=True)
    reward = "r1"
    multiple_runs(
        MultipleArgs(
            "ppo",
            "examples/pdptw/testingall.txt",
            reward,
            n_jobs=2 * len(GPUS),
            timeout=900,
            n_repeats=10,
            seed=10,
            require_gpu=True,
            logdir=f"logs/ppo-fine-tuned-pdptw-{reward}",
            reuse_gpu=True,
        )
    )


if __name__ == "__main__":
    try:
        dotenv.load_dotenv()
        tap.Parser(Args).bind(main).run()
    except Exception as e:
        logging.error(f"An error occurred: {e}", exc_info=True)
    finally:
        logging.info("Runner finished execution.")
