from typing import Literal, Optional
import torch
from runner import run
import typed_argparse as tap
from bridge import SocketBridge, NamedPipeBridge, Bridge
import logging


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
    _ddqn: str = tap.arg("--ddqn", help="Use Double DQN", default="false")
    lr: float = tap.arg("--lr", help="Learning rate", type=float, default=1e-4)
    keepalive: bool = tap.arg("--keepalive", help="Keep the connection alive", default=False)

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
        if self._device != "auto":
            return torch.device(self._device)
        n_devices = torch.cuda.device_count()
        if n_devices == 0:
            device = torch.device("cpu")
        else:
            try:
                with open("device", "r") as f:
                    device_num = int(f.read().strip()) % n_devices
            except Exception:
                device_num = 1
            device_num = max(1, device_num)  # Do not use device 0
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
    try:
        run(args)
    except Exception as e:
        logging.error(f"An error occurred: {e}", exc_info=True)


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
