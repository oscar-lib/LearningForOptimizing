from typing import Any
import torch
from abc import ABC, abstractmethod


class Problem[T](ABC):
    @abstractmethod
    def build_agent_input(self, data: dict[str, Any], device: torch.device) -> T:
        """Build the agent input from the data received from the bridge."""
