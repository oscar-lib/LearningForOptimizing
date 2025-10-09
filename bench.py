import torch
import timeit


SIZE = 512
REWARDS = torch.randn(SIZE)
DONES = torch.randint(0, 2, (SIZE,), dtype=torch.bool)
VALUES = torch.randn(SIZE)
NEXT_VALUES = torch.randn(SIZE)


def compute_gae(
    gamma: float, dones: torch.Tensor, rewards: torch.Tensor, values: torch.Tensor, next_values: torch.Tensor, trace_decay: float = 0.95
) -> torch.Tensor:
    deltas: list[float] = (rewards + gamma * next_values - values).tolist()
    gae = 0.0
    not_dones: list[float] = (~dones).float().tolist()
    advantages = torch.empty_like(rewards, dtype=torch.float32)
    for t in range(SIZE - 1, -1, -1):
        gae = deltas[t] + not_dones[t] * gamma**t * trace_decay * gae
        advantages[t] = gae
    return advantages


def compute_gae_lists(
    gamma: float, dones: torch.Tensor, rewards: torch.Tensor, values: torch.Tensor, next_values: torch.Tensor, trace_decay: float = 0.95
) -> torch.Tensor:
    deltas: list[float] = (rewards + gamma * next_values - values).tolist()
    gae = 0.0
    not_dones: list[float] = (~dones).float().tolist()
    advantages = [0.0] * SIZE
    for t in range(SIZE - 1, -1, -1):
        gae = deltas[t] + not_dones[t] * gamma**t * trace_decay * gae
        advantages[t] = gae
    return torch.tensor(advantages)


def compute_gae_lists_reversed(
    gamma: float, dones: torch.Tensor, rewards: torch.Tensor, values: torch.Tensor, next_values: torch.Tensor, trace_decay: float = 0.95
) -> torch.Tensor:
    deltas: list[float] = (rewards + gamma * next_values - values).tolist()
    gae = 0.0
    not_dones: list[float] = (~dones).float().tolist()
    advantages = []
    for t in range(SIZE - 1, -1, -1):
        gae = deltas[t] + not_dones[t] * gamma**t * trace_decay * gae
        advantages.append(gae)
    advantages.reverse()
    return torch.tensor(advantages)


def compute_gae_lists_reversed_zip(
    gamma: float, dones: torch.Tensor, rewards: torch.Tensor, values: torch.Tensor, next_values: torch.Tensor, trace_decay: float = 0.95
) -> torch.Tensor:
    deltas = reversed((rewards + gamma * next_values - values).tolist())
    gae = 0.0
    not_dones = reversed((~dones).float().tolist())
    advantages = []
    for delta, not_done, t in zip(deltas, not_dones, range(SIZE - 1, -1, -1)):
        gae = delta + not_done * gamma**t * trace_decay * gae
        advantages.append(gae)
    advantages.reverse()
    return torch.tensor(advantages)


def compute_gae_tensors(
    gamma: float,
    dones: torch.Tensor,
    rewards: torch.Tensor,
    values: torch.Tensor,
    next_values: torch.Tensor,
    trace_decay: float = 0.95,
) -> torch.Tensor:
    deltas = REWARDS + gamma * next_values - values
    gae = 0.0
    not_dones = (~dones).float()
    advantages = torch.empty_like(rewards, dtype=torch.float32)
    for t in range(SIZE - 1, -1, -1):
        gae = deltas[t] + not_dones[t] * gamma**t * trace_decay * gae
        advantages[t] = gae
    return advantages


if __name__ == "__main__":
    number = 1000
    t1 = timeit.timeit(lambda: compute_gae(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), number=number)
    t2 = timeit.timeit(lambda: compute_gae_tensors(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), number=number)
    t3 = timeit.timeit(lambda: compute_gae_lists(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), number=number)
    t4 = timeit.timeit(lambda: compute_gae_lists_reversed(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), number=number)
    t5 = timeit.timeit(lambda: compute_gae_lists_reversed_zip(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), number=number)
    print(f"compute_gae: {t1 / number:.6f} s per call")
    print(f"compute_gae_tensors: {t2 / number:.6f} s per call")
    print(f"compute_gae_lists: {t3 / number:.6f} s per call")
    print(f"compute_gae_lists_reversed: {t4 / number:.6f} s per call")
    print(f"compute_gae_lists_reversed_zip: {t5 / number:.6f} s per call")
    assert torch.allclose(
        compute_gae(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), compute_gae_tensors(0.99, DONES, REWARDS, VALUES, NEXT_VALUES)
    )
    assert torch.allclose(
        compute_gae(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), compute_gae_lists(0.99, DONES, REWARDS, VALUES, NEXT_VALUES)
    )
    assert torch.allclose(
        compute_gae(0.99, DONES, REWARDS, VALUES, NEXT_VALUES), compute_gae_lists_reversed(0.99, DONES, REWARDS, VALUES, NEXT_VALUES)
    )
    assert torch.allclose(
        compute_gae(0.99, DONES, REWARDS, VALUES, NEXT_VALUES),
        compute_gae_lists_reversed_zip(0.99, DONES, REWARDS, VALUES, NEXT_VALUES),
    )
    print("All implementations produce the same result.")
