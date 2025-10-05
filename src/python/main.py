import logging
import os
import random
from datetime import datetime
from functools import cached_property
from typing import Literal, Optional

import dotenv
import numpy as np
import torch
import typed_argparse as tap
from bridge import Bridge, NamedPipeBridge, SocketBridge, UnixSocketBridge
from runner import run
from utils import gpu


class Args(tap.TypedArgs):
    communication: Literal["socket", "pipe", "unix-socket"] = tap.arg("-c", help="Communication method")
    port: Optional[int] = tap.arg("-p", help="Port number", type=int)
    input_pipe: Optional[str] = tap.arg("-i", help="Input pipe name")
    output_pipe: Optional[str] = tap.arg("-o", help="Output pipe name")
    algorithm: Literal["dqn", "ppo"] = tap.arg("-a", help="Algorithm to use", default="dqn")
    _device: Literal["cpu", "auto", "auto-gpu"] | int | str = tap.arg("--device", help="Device to use", default="auto")
    epsilon: float = tap.arg("--epsilon", help="Epsilon value", type=float, default=0.1)
    _clipping: str | float = tap.arg("--clipping", help="Clipping value", default=0.0)
    batch_size: int = tap.arg("--batch-size", help="Batch size", default=32)
    memory_size: int = tap.arg("--memory-size", help="Size of the replay memory", default=10_000)
    ddqn: bool = tap.arg("--ddqn", help="Use Double DQN", default=False)
    lr: float = tap.arg("--lr", help="Learning rate", type=float, default=1e-4)
    keepalive: bool = tap.arg("--keepalive", help="Keep the connection alive", default=False)
    save_to: Optional[str] = tap.arg("--save-to", help="Path to save the model", default=None)
    load_from: Optional[str] = tap.arg("--load-from", help="Path to load the model from", default=None)
    no_train: bool = tap.arg("--no-train", help="Whether to train the model or not", default=False)
    seed: int = tap.arg("--seed", help="Random seed for reproducibility", default=0)
    no_target: bool = tap.arg("--no-target", help="Whether to use a target network for DQN", default=False)
    _logdir: Optional[str] = tap.arg("--logdir", help="Directory to save logs", default=None)
    disable_training_logs: bool = tap.arg("--disable-training-logs", help="Disable logging of training metrics", default=False)

    @cached_property
    def creation_time(self) -> str:
        return datetime.now().isoformat().replace(":", "-")

    @property
    def logdir(self) -> str:
        if self._logdir is None:
            return os.path.join("logs", self.creation_time)
        return self._logdir

    @property
    def clipping(self) -> Optional[float]:
        match self._clipping:
            case str():
                clip = float(self._clipping)
            case float():
                clip = self._clipping
            case _:
                raise ValueError(f"Invalid clipping value: {self._clipping}")
        if clip < 0:
            raise ValueError(f"Clipping value must be non-negative, got {clip}")
        if clip == 0:
            return None
        return clip

    @property
    def train(self):
        return not self.no_train

    @cached_property
    def device(self) -> torch.device:
        if self._device not in ("auto", "auto-gpu"):
            return torch.device(self._device)

        if self._device == "auto-gpu":
            if not torch.cuda.is_available():
                logging.error("CUDA is not available when using 'auto-gpu', falling back to CPU", exc_info=True)
                raise ValueError("CUDA is not available")
            device = gpu.get_device("auto", fit_strategy="scatter", estimated_memory_MB=2048)
        else:
            device = gpu.get_device("auto", fit_strategy="scatter", estimated_memory_MB=2048)
        logging.info(f"Using device: {device}")
        return device

    def make_bridge(self) -> Bridge:
        match self.communication:
            case "socket":
                if self.port is None:
                    raise Exception("Port is required for socket communication")
                return SocketBridge(self.port)
            case "pipe":
                if self.input_pipe is None or self.output_pipe is None:
                    raise Exception("Input and output pipes are required for pipe communication")
                return NamedPipeBridge(self.input_pipe, self.output_pipe)
            case "unix-socket":
                if self.input_pipe is None:
                    raise Exception("Address is required for unix-socket communication with the -i argument")
                return UnixSocketBridge(self.input_pipe)
            case other:
                raise Exception(f"Unknown communication method: {other}")


def main(args: Args):
    os.makedirs(args.logdir, exist_ok=True)
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(process)d - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"{args.logdir}/output-{args.seed}.log")],
    )
    logging.info(f"Starting the runner with arguments {args}:")
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)
    random.seed(args.seed)
    run(args)


if __name__ == "__main__":
    try:
        tap.Parser(Args).bind(main).run()
    except Exception as e:
        logging.error(f"An error occurred: {e}", exc_info=True)
    finally:
        logging.info("Runner finished execution.")
