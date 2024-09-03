from typing import Literal, Optional
from runner import Runner
import typed_argparse as tap
from bridge import SocketBridge, NamedPipeBridge


class Args(tap.TypedArgs):
    communication: Literal["socket", "pipe"] = tap.arg("-c", help="Communication method")
    port: Optional[int] = tap.arg("-p", help="Port number", type=int)
    input_pipe: Optional[str] = tap.arg("-i", help="Input pipe name")
    output_pipe: Optional[str] = tap.arg("-o", help="Output pipe name")
    algorithm: Literal["dqn", "ppo"] = tap.arg("-a", help="Algorithm to use", default="dqn")

    def validate(self):
        if self.communication == "pipe" and (self.input_pipe is None or self.output_pipe is None):
            raise Exception("Input and output pipes are required for pipe communication")
        if self.algorithm == "ppo":
            raise NotImplementedError("PPO is not implemented yet")


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

    server = Runner(bridge)
    server.run()


if __name__ == "__main__":
    tap.Parser(Args).bind(main).run()
