from typing import Literal, Optional

import torch
from time import time
from runner import Runner
import typed_argparse as tap
from bridge import SocketBridge, NamedPipeBridge


class Args(tap.TypedArgs):
    communication: Literal["socket", "pipe"] = tap.arg("-c", help="Communication method")
    port: Optional[int] = tap.arg("-p", help="Port number", type=int)
    input_pipe: Optional[str] = tap.arg("-i", help="Input pipe name")
    output_pipe: Optional[str] = tap.arg("-o", help="Output pipe name")
    algorithm: Literal["dqn", "ppo"] = tap.arg("-a", help="Algorithm to use", default="dqn")
    device: Literal["gpu", "cpu"] = tap.arg("-d", help="Device to use", default="cpu")


def main(args: Args):
    match args.communication:
        case "socket":
            if args.port is None:
                raise Exception("Port is required for socket communication")
            bridge = SocketBridge(args.port)
        case "pipe":
            if args.input_pipe is None or args.output_pipe is None:
                raise Exception("Input and output pipes are required for pipe communication")
            bridge = NamedPipeBridge(args.input_pipe, args.output_pipe)
        case other:
            raise Exception(f"Unknown communication method: {other}")
    if args.device == "gpu":
        n_devices = torch.cuda.device_count()
        if n_devices == 0:
            device = torch.device("cpu")
        else:
            device_index = int(time() * 1000) % n_devices
            device = torch.device(f"cuda:{device_index}")
    else:
        device = torch.device("cpu")

    server = Runner(bridge)
    server.run(device, args.algorithm)


if __name__ == "__main__":
    tap.Parser(Args).bind(main).run()
