from typing import Literal, Optional
import torch
import random
import numpy as np
from runner import run
import typed_argparse as tap
from bridge import SocketBridge, NamedPipeBridge, Bridge
import logging
import dotenv
import os
from datetime import datetime


class Args(tap.TypedArgs):
    communication: Literal["socket", "pipe"] = tap.arg("-c", help="Communication method")
    port: Optional[int] = tap.arg("-p", help="Port number", type=int)
    input_pipe: Optional[str] = tap.arg("-i", help="Input pipe name")
    output_pipe: Optional[str] = tap.arg("-o", help="Output pipe name")
    algorithm: Literal["dqn", "ppo"] = tap.arg("-a", help="Algorithm to use", default="dqn")
    _device: Literal["cpu", "auto"] | int | str = tap.arg("--device", help="Device to use", default="auto")
    epsilon: float = tap.arg("--epsilon", help="Epsilon value", type=float, default=0.1)
    _clipping: str | float = tap.arg("--clipping", help="Clipping value", default=0.0)
    batch_size: int = tap.arg("--batch-size", help="Batch size", default=32)
    memory_size: int = tap.arg("--memory-size", help="Size of the replay memory", default=1_000)
    ddqn: bool = tap.arg("--ddqn", help="Use Double DQN", default=False)
    lr: float = tap.arg("--lr", help="Learning rate", type=float, default=1e-4)
    keepalive: bool = tap.arg("--keepalive", help="Keep the connection alive", default=False)
    save_to: Optional[str] = tap.arg("--save-to", help="Path to save the model", default=None)
    load_from: Optional[str] = tap.arg("--load-from", help="Path to load the model from", default=None)
    no_train: bool = tap.arg("--no-train", help="Whether to train the model or not", default=False)
    seed: int = tap.arg("--seed", help="Random seed for reproducibility", default=0)
    with_distance_matrix: bool = tap.arg(
        "--with-distance-matrix", help="Whether to compute and use the distance matrix as node attribute for PDPTW or TSP", default=False
    )

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

    @property
    def device(self) -> torch.device:
        if self._device != "auto":
            return torch.device(self._device)
        n_devices = torch.cuda.device_count()
        if n_devices == 0:
            return torch.device("cpu")
        try:
            with open("device", "r") as f:
                device_num = int(f.read().strip()) % n_devices
        except Exception:
            device_num = 0
        next_device = (device_num + 1) % n_devices
        with open("device", "w") as f:
            f.write(f"{next_device}")
        device = torch.device(device_num)
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
            case other:
                raise Exception(f"Unknown communication method: {other}")


def main(args: Args):
    logging.info(f"Starting the runner with arguments {args}:")
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)
    random.seed(args.seed)
    run(args)


if __name__ == "__main__":
    os.makedirs("logs", exist_ok=True)
    dotenv.load_dotenv()
    logging.basicConfig(
        level=os.getenv("LOG_LEVEL", "INFO").upper(),
        format="%(asctime)s - %(process)d - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler(f"logs/{datetime.now().isoformat()}.log")],
    )
    try:
        tap.Parser(Args).bind(main).run()
    except Exception as e:
        logging.error(f"An error occurred: {e}", exc_info=True)
    finally:
        logging.info("Runner finished execution.")
