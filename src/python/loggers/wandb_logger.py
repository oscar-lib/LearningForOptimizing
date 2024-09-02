import os

import wandb

from .abstract_logger import AbstractLogger


class WandbLogger(AbstractLogger):
    def __init__(self, logdir: str, quiet=False) -> None:
        super().__init__(logdir, quiet)
        api_key = os.environ.get("WANDB_KEY", None)
        project = os.environ.get("WANDB_PROJECT", None)
        if api_key is None or project is None:
            raise Exception("WANDB_KEY and WANDB_PROJECT must be set in the environment variables")

        wandb.init(
            project=project,
            dir=logdir,
            # track hyperparameters and run metadata
            config={},
        )

    def log(self, data: dict[str, float], time_step: int):
        return wandb.log(data, step=time_step)
