import logging
import os
import random
import orjson
from datetime import datetime
from functools import cached_property
from typing import Literal, Optional
from marlenv.utils import Schedule

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
    epsilon_end: float = tap.arg("--epsilon-end", help="Epsilon value", type=float, default=0.1)
    epsilon_start: float = tap.arg("--epsilon-start", help="Starting value of epsilon for linear decay", type=float, default=1.0)
    epsilon_n_secs: int = tap.arg("--epsilon-n-secs", help="Number of seconds over which epsilon is decayed", type=int, default=300)
    epsilon_decay: Literal["linear", "exponential"] = tap.arg("--epsilon-decay", help="Epsilon decay strategy", default="linear")
    clipping: Optional[float] = tap.arg("--clipping", help="Clipping value", default=None)
    batch_size: int = tap.arg("--batch-size", help="Batch size", default=32)
    memory_size: int = tap.arg("--memory-size", help="Size of the replay memory", default=10_000)
    ddqn: bool = tap.arg("--ddqn", help="Use Double DQN", default=False)
    lr: float = tap.arg("--lr", help="Learning rate", type=float, default=1e-4)
    lr_critic: float = tap.arg("--lr-critic", help="Learning rate for the critic in PPO", type=float, default=1e-5)
    keepalive: bool = tap.arg("--keepalive", help="Keep the connection alive", default=False)
    save_to: Optional[str] = tap.arg("--save-to", help="Path to save the model", default=None)
    load_from: Optional[str] = tap.arg("--load-from", help="Path to load the model from", default=None)
    no_train: bool = tap.arg("--no-train", help="Whether to train the model or not", default=False)
    seed: int = tap.arg("--seed", help="Random seed for reproducibility", default=0)
    no_target: bool = tap.arg("--no-target", help="Whether to use a target network for DQN", default=False)
    _logdir: Optional[str] = tap.arg("--logdir", help="Directory to save logs", default=None)
    disable_training_logs: bool = tap.arg("--disable-training-logs", help="Disable logging of training metrics", default=False)
    c1_start: float = tap.arg("--c1-start", help="Initial value of c1 for PPO", type=float, default=1.0)
    c1_end: float = tap.arg("--c1-end", help="Final value of c1 for PPO", type=float, default=0.0)
    c1_n_secs: int = tap.arg("--c1-n-secs", help="Number of seconds over which c1 is decayed", type=int, default=300)
    n_epochs: int = tap.arg("--n-epochs", help="Number of epochs per update for PPO", type=int, default=20)
    c2_start: float = tap.arg("--c2-start", help="Initial value of c2 for PPO", type=float, default=1.0)
    c2_end: float = tap.arg("--c2-end", help="Final value of c2 for PPO", type=float, default=0.0)
    c2_n_secs: int = tap.arg("--c2-n-secs", help="Number of seconds over which c2 is decayed", type=int, default=300)

    @cached_property
    def creation_time(self) -> str:
        return datetime.now().isoformat().replace(":", "-")

    @property
    def logdir(self) -> str:
        if self._logdir is None:
            return os.path.join("logs", self.creation_time)
        return self._logdir

    @property
    def epsilon(self):
        if self.epsilon_start == self.epsilon_end:
            return Schedule.constant(self.epsilon_start)
        if self.epsilon_decay == "linear":
            return Schedule.linear(self.epsilon_start, self.epsilon_end, self.epsilon_n_secs)
        elif self.epsilon_decay == "exponential":
            return Schedule.exp(self.epsilon_start, self.epsilon_end, self.epsilon_n_secs)
        raise ValueError(f"Unknown epsilon decay strategy: {self.epsilon_decay}")

    @property
    def train(self):
        return not self.no_train

    @property
    def c1(self):
        if self.c1_start == self.c1_end:
            return Schedule.constant(self.c1_start)
        return Schedule.linear(self.c1_start, self.c1_end, self.c1_n_secs)

    @property
    def c2(self):
        if self.c2_start == self.c2_end:
            return Schedule.constant(self.c2_start)
        return Schedule.linear(self.c2_start, self.c2_end, self.c2_n_secs)

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
        format="%(asctime)s - %(process)d - %(levelname)s - %(filename)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"{args.logdir}/output-{args.seed}.log")],
    )
    logging.info(f"Starting the runner with arguments {args}:")
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)
    random.seed(args.seed)
    with open(os.path.join(args.logdir, f"args-{args.seed}.json"), "wb") as f:
        f.write(orjson.dumps(args.__dict__, option=orjson.OPT_INDENT_2))
    run(args)


if __name__ == "__main__":
    try:
        tap.Parser(Args).bind(main).run()
    except Exception as e:
        logging.error(f"An error occurred: {e}", exc_info=True)
    finally:
        logging.info("Runner finished execution.")
