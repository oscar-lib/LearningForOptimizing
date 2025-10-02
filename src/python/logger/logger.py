import os
from .abstract_logger import AbstractLogger
from .csv_logger import CSVLogger


class Logger(AbstractLogger):
    def __init__(self, logdir: str, quiet=False) -> None:
        self.loggers = list[AbstractLogger]()
        super().__init__(logdir, quiet)
        self.loggers.append(CSVLogger(os.path.join(self.logdir, "metrics.csv"), quiet))

    def log(self, data: dict[str, float], time_step: int):
        for logger in self.loggers:
            logger.log(data, time_step)

    def __del__(self):
        for logger in self.loggers:
            del logger

    def close(self):
        for logger in self.loggers:
            logger.close()
