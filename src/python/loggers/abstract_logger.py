import logging
import os
import shutil
from abc import ABC, abstractmethod

from dotenv import load_dotenv

load_dotenv()


class AbstractLogger(ABC):
    """Logging interface"""

    def __init__(self, logdir: str, quiet=False) -> None:
        if not logdir.startswith("logs/"):
            logdir = os.path.join("logs", logdir)
        self.logdir: str = logdir
        if os.path.exists(logdir):
            if os.path.basename(logdir).lower() in ["test", "debug"]:
                shutil.rmtree(logdir)
        os.makedirs(logdir, exist_ok=True)
        level = os.environ.get("LOG_LEVEL", None)
        if level:
            level = level.upper()
            print(f"Setting log level to {level}")
            logging.basicConfig(level=level, filename=os.path.join(logdir, "log.txt"), filemode="w")
        self.quiet = quiet

    @abstractmethod
    def log(self, data: dict[str, float], time_step: int):
        """Log data"""

    @staticmethod
    def error(msg: str):
        """Log an error message"""
        logging.error(msg)

    @staticmethod
    def warning(msg: str):
        """Log a warning message"""
        logging.warning(msg)

    @staticmethod
    def info(msg: str):
        """Log an info message"""
        logging.info(msg)

    def close(self):
        """Close the logger"""
