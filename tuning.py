from datetime import datetime
import os
import dotenv
import optuna
import logging
import subprocess
from run_experiments import multiple_runs, MultipleArgs


def run(trial: optuna.Trial):
    args = MultipleArgs(
        bandit="dqn-no-target",
        problems_file="examples/csp/training_subset.txt",
        reward="r2",
        n_jobs=16,
        timeout=300,
        n_repeats=3,
        output_filename=None,
        args={
            "learningRate": trial.suggest_float("lr", 1e-5, 1e-2, log=True),
            "batchSize": trial.suggest_int("batchSize", 32, 256),
            "epsilon": trial.suggest_float("epsilon", 0.01, 1.0, log=True),
            "clipping": trial.suggest_float("clipping", 0.5, 50.0),
            "memorySize": trial.suggest_int("memory size", 1_000, 100_000, step=1_000),
        },
    )
    logging.info(args)
    results = multiple_runs(args)
    avg = sum(result.integral_primal_gap for result in results) / len(results)
    return avg


if __name__ == "__main__":
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(process)d - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"{datetime.now().isoformat()}.log")],
    )
    subprocess.run("sbt assembly", shell=True, check=True)
    study = optuna.create_study(direction="minimize", study_name="CSP-r2-no_target", storage="sqlite:///tuning.db", load_if_exists=True)
    study.optimize(run, n_trials=100)
