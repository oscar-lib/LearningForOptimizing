from datetime import datetime
import os
import subprocess
import dotenv
import optuna
import logging
import shutil
import typed_argparse as tap
from run_experiments import multiple_runs, MultipleArgs
from optuna.trial import TrialState


def ppo_parameters(trial: optuna.Trial, timeout: int):
    c1_start = trial.suggest_float("c1_start", 0.0, 1.0, step=0.01)
    c1_end = trial.suggest_float("c1_end", 0.0, 1.0, step=0.01)
    c2_start = trial.suggest_float("c2_start", 0.0, 1.0, step=0.01)
    c2_end = trial.suggest_float("c2_end", 0.0, c2_start, step=0.01)
    memory_size = trial.suggest_int("memory size", 10, 1_000, step=5)
    return {
        "learningRate": trial.suggest_float("lr actor", 1e-5, 1e-2, log=True),
        "lrCritic": trial.suggest_float("lrCritic", 1e-5, 1e-2, log=True),
        "batchSize": trial.suggest_int("batchSize", 5, memory_size // 2, step=10),
        "clipping": trial.suggest_float("clipping", 0.0, 50.0),
        "memorySize": memory_size,
        "c1Start": c1_start,
        "c1End": c1_end,
        "c1NSecs": trial.suggest_int("c1NSecs", 1, timeout, step=5),
        "c2Start": c2_start,
        "c2End": c2_end,
        "c2NSecs": trial.suggest_int("c2NSecs", 1, timeout, step=5),
        "nEpochs": trial.suggest_int("nEpochs", 1, 100),
    }


def dqn_parameters(trial: optuna.Trial, timeout: int):
    eps_start = trial.suggest_float("epsilon_start", 0.5, 1.0, step=0.01)
    return {
        "learningRate": trial.suggest_float("lr", 1e-5, 1e-2, log=True),
        "batchSize": trial.suggest_int("batchSize", 16, 256, step=32),
        "clipping": trial.suggest_float("clipping", 0.0, 50.0),
        "memorySize": trial.suggest_int("memory size", 500, 3_000, step=100),
        "epsilonStart": eps_start,
        "epsilonEnd": trial.suggest_float("epsilon_end", 0.0, 0.5, step=0.01),
        "epsilonNSecs": trial.suggest_int("epsilon_n_secs", 0, timeout, step=5),
        "epsilonDecay": "linear",
        "ddqn": trial.suggest_categorical("ddqn", (True, False)),
    }


class Args(tap.TypedArgs):
    no_compile: bool = tap.arg("--no-compile", help="Skip compilation step", default=False)

    @property
    def compile(self):
        return not self.no_compile


def main(args: Args):
    if args.compile:
        subprocess.run("sbt assembly", shell=True, check=True)
    for bandit in ("dqn", "ppo"):
        for problem in ("tsp", "pdptw", "csp"):
            if problem == "csp":
                timeout = 900
            else:
                timeout = 300
            for reward in ("r1", "r2", "r3"):

                def run(trial: optuna.Trial):
                    assert reward in ("r1", "r2", "r3")
                    match bandit:
                        case "dqn":
                            params = dqn_parameters(trial, timeout)
                        case "ppo":
                            params = ppo_parameters(trial, timeout)
                        case other:
                            raise NotImplementedError(f"Not implemented for {other}")
                    args = MultipleArgs(
                        bandit=bandit,
                        problems_file=f"examples/{problem}/training_subset.txt",
                        reward=reward,
                        n_jobs=24,
                        timeout=timeout,
                        n_repeats=2,
                        require_gpu=True,
                        args=params,
                        reuse_gpu=True,
                        tolerate_failures=False,
                    )
                    logging.info(args)
                    try:
                        results, failures = multiple_runs(args)
                    except Exception as e:
                        logging.error(f"Trial {trial.number} failed with exception: {e}", exc_info=True)
                        return float("inf")
                    shutil.rmtree(args.logdir)
                    total = 0.0
                    mmax = 0
                    for result in results:
                        if problem == "csp":
                            if not result.is_optimal():
                                total += result.best_obj * result.n_secs_to_best_obj
                                mmax = max(mmax, result.n_secs_to_best_obj)
                        else:
                            total += result.integral_primal_gap
                            mmax = max(mmax, result.integral_primal_gap)
                    if len(failures) > 0:
                        # Add the maximum penalty for failures
                        penalty = mmax * len(failures)
                        logging.warning(
                            f"Trial {trial.number} had {len(failures)} failures, adding penalty of {mmax} x {len(failures)} = {penalty}"
                        )
                        total += penalty
                    return total

                study = optuna.create_study(
                    direction="minimize",
                    study_name=f"{bandit.upper()}-{problem.upper()}-{reward.upper()}",
                    storage="sqlite:///tuning.db",
                    load_if_exists=True,
                )
                remaining = 100 - len([t for t in study.trials if t.state == TrialState.COMPLETE])
                study.optimize(run, n_trials=remaining)


if __name__ == "__main__":
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"tuning-{datetime.now().isoformat()}.log")],
    )
    tap.Parser(Args).bind(main).run()
