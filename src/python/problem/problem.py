from typing import Any, Self
import torch
from abc import ABC, abstractmethod


class Problem[T](ABC):
    @abstractmethod
    def build_agent_input(self, data: dict[str, Any], device: torch.device) -> T:
        """Build the agent input from the data received from the bridge."""

    @abstractmethod
    def is_compatible_with(self, other: Self) -> bool:
        """Check if the problem is compatible with another problem instance with regard to the NN architecture."""
