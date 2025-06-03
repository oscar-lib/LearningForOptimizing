from typing import Literal, Optional
import os
import traceback
import torch
from runner import Runner, Params
import typed_argparse as tap
from bridge import SocketBridge, NamedPipeBridge, Bridge
import logging


class Args(tap.TypedArgs):
    communication: Literal["socket", "pipe"] = tap.arg("-c", help="Communication method")
    port: Optional[int] = tap.arg("-p", help="Port number", type=int)
    input_pipe: Optional[str] = tap.arg("-i", help="Input pipe name")
    output_pipe: Optional[str] = tap.arg("-o", help="Output pipe name")
    algorithm: Literal["dqn", "ppo"] = tap.arg("-a", help="Algorithm to use", default="dqn")
    _device: Literal["gpu", "cpu"] = tap.arg("--device", help="Device to use", default="gpu")
    epsilon: float = tap.arg("--epsilon", help="Epsilon value", type=float, default=0.1)
    _clipping: str | float = tap.arg("--clipping", help="Clipping value", default=0.0)
    batch_size: int = tap.arg("--batch-size", help="Batch size", default=32)
    _ddqn: str = tap.arg("--ddqn", help="Use Double DQN", default="false")
    lr: float = tap.arg("--lr", help="Learning rate", type=float, default=1e-4)

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
    def ddqn(self) -> bool:
        return self._ddqn.lower() == "true"

    @property
    def device(self) -> torch.device:
        n_devices = torch.cuda.device_count()
        logging.info(f"Number of available CUDA devices: {n_devices}")
        if n_devices == 0:
            return torch.device("cpu")
        device_index = os.getpid() % n_devices
        return torch.device(f"cuda:{device_index}")

    @property
    def bridge(self) -> Bridge:
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

    def to_params(self):
        return Params(lr=self.lr, ddqn=self.ddqn, batch_size=self.batch_size, clipping=self.clipping, epsilon=self.epsilon)


def main(args: Args):
    logging.info(f"Starting the runner with arguments {args}:")
    try:
        runner = Runner(args.bridge)
        print(args)
        runner.run(args.device, args.algorithm, args.to_params())
    except Exception as e:
        logging.error(f"An error occurred: {e}")
        logging.error(traceback.format_exc())
        traceback.print_exc()


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(), logging.FileHandler("logs.log")],
    )
    try:
        logging.info("Binding arguments")
        tap.Parser(Args).bind(main).run()
        logging.info("End of the program")
    except Exception as e:
        logging.error(f"Argument parsing error: {e}")
