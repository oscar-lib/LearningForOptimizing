from datetime import datetime
import os
import subprocess
import dotenv
import optuna
import logging
import shutil
from run_experiments import multiple_runs, MultipleArgs


def ppo_parameters(trial: optuna.Trial, timeout: int):
    c1_start = trial.suggest_float("c1_start", 0.0, 1.0, step=0.01)
    c1_end = trial.suggest_float("c1_end", 0.0, 1.0, step=0.01)
    c2_start = trial.suggest_float("c2_start", 0.0, 1.0, step=0.01)
    c2_end = trial.suggest_float("c2_end", 0.0, c2_start, step=0.01)
    memory_size = trial.suggest_int("memory size", 20, 2000, step=5)
    return {
        "learningRate": trial.suggest_float("lr actor", 1e-5, 1e-2, log=True),
        "lrCritic": trial.suggest_float("lrCritic", 1e-5, 1e-2, log=True),
        "batchSize": trial.suggest_int("batchSize", 5, memory_size // 2, step=10),
        "clipping": trial.suggest_float("clipping", 0.0, 50.0),
        "memorySize": memory_size,
        "c1Start": c1_start,
        "c1End": c1_end,
        "c1NSecs": trial.suggest_int("epsilon_n_secs", 1, timeout, step=5),
        "c2Start": c2_start,
        "c2End": c2_end,
        "c2NSecs": trial.suggest_int("c2_n_secs", 1, timeout, step=5),
        "nEpochs": trial.suggest_int("n_epochs", 1, 100),
    }


def dqn_parameters(trial: optuna.Trial, timeout: int):
    eps_start = trial.suggest_float("epsilon_start", 0.0, 1.0)
    eps_end = trial.suggest_float("epsilon_end", 0.0, eps_start, step=0.01)
    return {
        "learningRate": trial.suggest_float("lr", 1e-5, 1e-2, log=True),
        "batchSize": trial.suggest_int("batchSize", 128, 256, step=32),
        "clipping": trial.suggest_float("clipping", 0.0, 50.0),
        "memorySize": trial.suggest_int("memory size", 20_000, 100_000, step=1_000),
        "epsilonStart": eps_start,
        "epsilonEnd": eps_end,
        "epsilonNSecs": trial.suggest_int("epsilon_n_secs", 0, timeout, step=5),
        "epsilonDecay": trial.suggest_categorical("epsilon_decay", ("linear", "exponential")),
    }


def run(trial: optuna.Trial):
    timeout = 900
    args = ppo_parameters(trial, timeout)
    args = MultipleArgs(
        bandit="ppo",
        problems_file="examples/csp/training-500.txt",
        reward="r2",
        n_jobs=16,
        timeout=timeout,
        n_repeats=3,
        args=args,
    )
    logging.info(args)
    results = multiple_runs(args)
    shutil.rmtree(args.logdir)
    total = 0.0
    for result in results:
        if not result.is_optimal():
            total += result.objective(timeout) * result.n_secs_to_best_obj
    return total


if __name__ == "__main__":
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"tuning-{datetime.now().isoformat()}.log")],
    )
    subprocess.run("sbt assembly", shell=True, check=True)
    study = optuna.create_study(
        direction="minimize", study_name="PPO-CSP-500", storage="sqlite:///tuning.db", load_if_exists=True
    )  # , sampler=RandomSampler())
    study.optimize(run, n_trials=100)
