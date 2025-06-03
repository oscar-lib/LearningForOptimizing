from typing import Any
from abc import ABC, abstractmethod


class Problem[T](ABC):
    @abstractmethod
    def build_agent_input(self, data: dict[str, Any]) -> T:
        """Build the agent input from the data received from the bridge."""
