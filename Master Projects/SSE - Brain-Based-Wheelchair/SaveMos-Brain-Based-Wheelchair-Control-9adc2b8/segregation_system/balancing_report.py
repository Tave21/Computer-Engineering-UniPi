"""
Author: Saverio Mosti
Creation Date: 2024-12-06
"""
from typing import List

from segregation_system.prepared_session import PreparedSession


class BalancingReport:
    """
    Represents a report on balancing actions, including the counts for moves,
    turns to the left, and turns to the right.

    Attributes:
        _move (int): The number of moves in the balancing process.
        _turn_left (int): The number of times a turn to the left occurred.
        _turn_right (int): The number of times a turn to the right occurred.

    Author: Saverio Mosti

    Creation Date: 2024-12-06
    """

    def __init__(self, sessions: List[PreparedSession]):
        """
        Initializes a new instance of the `BalancingReport` class.
        """
        self._move = 0
        self._turn_left = 0
        self._turn_right = 0

        for session in sessions:
            if session.label == "move":
                self._move += 1
            if session.label == "turn_left":
                self._turn_left += 1
            if session.label == "turn_right":
                self._turn_right += 1

    # Getter and setter for move
    @property
    def move(self) -> int:
        """
        Gets the number of moves.

        Returns:
            int: The number of moves.
        """
        return self._move

    @move.setter
    def move(self, value: int):
        """
        Sets the number of moves.

        Args:
            value (int): The new number of moves.

        Raises:
            ValueError: If the value is not a non-negative integer.
        """
        if not isinstance(value, int) or value < 0:
            raise ValueError("move must be a non-negative integer.")
        self._move = value

    # Getter and setter for turn_left
    @property
    def turn_left(self) -> int:
        """
        Gets the number of left turns.

        Returns:
            int: The number of left turns.
        """
        return self._turn_left

    @turn_left.setter
    def turn_left(self, value: int):
        """
        Sets the number of left turns.

        Args:
            value (int): The new number of left turns.

        Raises:
            ValueError: If the value is not a non-negative integer.
        """
        if not isinstance(value, int) or value < 0:
            raise ValueError("turn_left must be a non-negative integer.")
        self._turn_left = value

    # Getter and setter for turn_right
    @property
    def turn_right(self) -> int:
        """
        Gets the number of right turns.

        Returns:
            int: The number of right turns.
        """
        return self._turn_right

    @turn_right.setter
    def turn_right(self, value: int):
        """
        Sets the number of right turns.

        Args:
            value (int): The new number of right turns.

        Raises:
            ValueError: If the value is not a non-negative integer.
        """
        if not isinstance(value, int) or value < 0:
            raise ValueError("turn_right must be a non-negative integer.")
        self._turn_right = value
