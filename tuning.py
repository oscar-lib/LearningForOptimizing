from datetime import datetime
import os
import dotenv
import optuna
import logging
import shutil
import subprocess
from run_experiments import multiple_runs, MultipleArgs


def run(trial: optuna.Trial):
    eps_start = trial.suggest_float("epsilon_start", 0.0, 1.0)
    eps_end = trial.suggest_float("epsilon_end", 0.0, eps_start, step=0.01)
    timeout = 600
    args = MultipleArgs(
        bandit="dqn-no-target",
        problems_file="examples/csp/training_subset.txt",
        reward="r2",
        n_jobs=16,
        timeout=timeout,
        n_repeats=3,
        output_filename=None,
        args={
            "learningRate": trial.suggest_float("lr", 1e-5, 1e-2, log=True),
            "batchSize": trial.suggest_int("batchSize", 128, 256, step=32),
            "clipping": trial.suggest_float("clipping", 0.0, 50.0),
            "memorySize": trial.suggest_int("memory size", 20_000, 100_000, step=1_000),
            "epsilonStart": eps_start,
            "epsilonEnd": eps_end,
            "epsilonNSecs": trial.suggest_int("epsilon_n_secs", 0, timeout, step=5),
            "epsilonDecay": trial.suggest_categorical("epsilon_decay", ("linear", "exponential")),
        },
    )
    logging.info(args)
    results = multiple_runs(args)
    avg = sum(result.integral_primal_gap for result in results) / len(results)
    shutil.rmtree(args.logdir)
    return avg


if __name__ == "__main__":
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(process)d - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"tuning-{datetime.now().isoformat()}.log")],
    )
    subprocess.run("sbt assembly", shell=True, check=True)
    study = optuna.create_study(
        direction="minimize", study_name="CSP_300", storage="sqlite:///tuning.db", load_if_exists=True
    )  # , sampler=RandomSampler())
    study.optimize(run, n_trials=100)
