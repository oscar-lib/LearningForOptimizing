from .abstract_logger import AbstractLogger
from .csv_logger import CSVLogger
from .wandb_logger import WandbLogger


class Logger(AbstractLogger):
    def __init__(
        self,
        logdir: str,
        csv: bool | CSVLogger = False,
        wandb: bool | WandbLogger = False,
        quiet=False,
    ) -> None:
        self.loggers = list[AbstractLogger]()
        assert csv, "At least one logger must be provided."
        super().__init__(logdir, quiet)
        if csv:
            if isinstance(csv, CSVLogger):
                self.loggers.append(csv)
            else:
                self.loggers.append(CSVLogger(logdir + "/log.csv", quiet))
        if wandb:
            if isinstance(wandb, WandbLogger):
                self.loggers.append(wandb)
            else:
                self.loggers.append(WandbLogger(logdir, quiet))

    def log(self, data: dict[str, float], time_step: int):
        for logger in self.loggers:
            logger.log(data, time_step)

    def __del__(self):
        for logger in self.loggers:
            del logger

    def close(self):
        for logger in self.loggers:
            logger.close()
