import logging
import os
from typing import Literal, TYPE_CHECKING

from algos import DQN, PPO, Algo
from bridge import Bridge
from bridge.protocol.message import Message, MessageType
from logger import CSVLogger
from optimenv import EpisodeEndException, OptimEnv
from problem import CSP, PDPTW, TSP, Problem
from replay_memory import GraphReplayMemory, LinearMemory

if TYPE_CHECKING:
    from main import Args


def do_run(agent: Algo, env: OptimEnv, logger: CSVLogger, train: bool):
    t = 0
    obs = env.reset()
    while True:
        t += 1
        action, action_data = agent.select_action(obs)
        logs = {"action": action, **{f"action-{i}": x for i, x in enumerate(action_data)}}
        try:
            next_obs, reward, new_obj = env.step(action)
            logs = logs | {"reward": reward, "obj": new_obj}
            if train:
                logs = logs | agent.learn(t, obs, action, reward, next_obs, new_obj)
            logger.log(logs, t)
            obs = next_obs
        except EpisodeEndException:
            logging.debug("Episode ended")
            if train:
                agent.notify_episode_end()
            obs = env.reset()


def run(args: "Args"):
    logger = CSVLogger(os.path.join(args.logdir, f"metrics-{args.seed}.csv"))
    device = args.device
    logger.info("Starting runner")
    problem = None
    agent = None
    bridge = None
    stop = False
    while not stop:
        logging.info("Waiting for a new connection")
        try:
            bridge = args.make_bridge()
            new_problem = _retrieve_problem_data(bridge, logger)
            if problem is None:
                problem = new_problem
            else:
                assert problem.is_compatible_with(new_problem)
            if agent is None:
                agent = _create_agent(problem, args.algorithm, args).to(device)
                if args.load_from is not None:
                    logging.info(f"Loading agent from {args.load_from}")
                    agent.load(args.load_from)
            env = OptimEnv(problem, bridge, device)
            do_run(agent, env, logger, args.train)
        except ConnectionResetError:
            logging.info("Connection with remote closed")
            stop = not args.keepalive
        except KeyboardInterrupt:
            logging.info("Stopping runner with Ctrl+C")
            stop = True
        except Exception as e:
            logging.error(f"An unexpected error occurred: {e}", exc_info=True)
            stop = True
        finally:
            if agent is not None and args.save_to is not None:
                agent.save(args.save_to)


def _retrieve_problem_data(bridge: Bridge, logger: CSVLogger) -> Problem:
    req = bridge.recv()
    match req.type:
        case MessageType.STATIC_DATA_PDPTW:
            problem = PDPTW.parse(req.body)
        case MessageType.STATIC_DATA_CSP:
            problem = CSP.parse(req.body)
        case MessageType.STATIC_DATA_TSP:
            problem = TSP.parse(req.body)
        case other:
            error = f"Expected message of type {[MessageType.STATIC_DATA_PDPTW, MessageType.STATIC_DATA_CSP, MessageType.STATIC_DATA_TSP]}  from the client, got {other}"
            logger.error(error)
            bridge.send(Message.error(error).to_bytes())
            raise Exception(error)
    bridge.send(Message.ack().to_bytes())
    return problem


def _create_agent(problem: Problem, algo: Literal["dqn", "ppo"], args: "Args") -> Algo:
    match algo:
        case "dqn":
            match problem:
                case PDPTW() | TSP():
                    from nn import QNetGNN

                    qnetwork = QNetGNN(problem)
                    memory = GraphReplayMemory(args.memory_size)
                case CSP():
                    from nn import CNN1D

                    qnetwork = CNN1D(problem)
                    memory = LinearMemory(args.memory_size)
                case other:
                    raise Exception(f"Unsupported problem for DQN: {other}")
            return DQN(
                qnetwork=qnetwork,
                memory=memory,
                double_qlearning=args.ddqn,
                grad_norm_clipping=args.clipping,
                lr=args.lr,
                epsilon=args.epsilon,
                batch_size=args.batch_size,
                device=args.device,
                use_target=args.use_target,
            )
        case "ppo":
            assert isinstance(problem, PDPTW)
            return PPO(
                problem=problem,
                lr_actor=0.001,
                lr_critic=0.001,
                gamma=0.99,
                K_epochs=20,
                eps_clip=0.2,
            )
    raise Exception(f"Unknown algorithm: {algo}")
