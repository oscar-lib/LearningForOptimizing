from .logger import Logger

__all__ = ["Logger"]

_logger = Logger(wandb=True)


def new_run(wandb=True, csv=True, logdir: str | None = None):
    global _logger
    _logger.close()
    _logger = Logger(wandb=wandb, csv=csv, logdir=logdir)


def log(data: dict[str, float], time_step: int):
    _logger.log(data, time_step)


def info(message: str):
    _logger.info(message)


def error(message: str):
    _logger.error(message)


def warning(message: str):
    _logger.warning(message)
