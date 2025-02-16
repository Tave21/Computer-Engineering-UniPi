"""
Author: Saverio Mosti
Creation Date: 2024-12-06
"""

from typing import List
from segregation_system.prepared_session import PreparedSession

class CoverageReport:
    """
    Represents a report on coverage, including a list of `PreparedSession` objects.

    Attributes:
        prepared_sessions (List[PreparedSession]): A list of `PreparedSession` objects used in the coverage report.

    Author: Saverio Mosti

    Creation Date: 2024-12-06
    """

    def __init__(self, prepared_sessions: List[PreparedSession]):
        """
        Initializes a new instance of the `CoverageReport` class.

        Args:
            prepared_sessions (List[PreparedSession]): A list of `PreparedSession` objects.
        """
        self._prepared_sessions = prepared_sessions

    # Getter for prepared_sessions
    @property
    def prepared_sessions(self) -> List[PreparedSession]:
        """
        Gets the list of prepared sessions.

        Returns:
            List[PreparedSession]: The list of `PreparedSession` objects.
        """
        return self._prepared_sessions

    # Setter for prepared_sessions
    @prepared_sessions.setter
    def prepared_sessions(self, value: List[PreparedSession]):
        """
        Sets the list of prepared sessions.

        Args:
            value (List[PreparedSession]): The new list of `PreparedSession` objects.

        Raises:
            ValueError: If the value is not a list of `PreparedSession` objects.
        """
        if not isinstance(value, list) or not all(isinstance(item, PreparedSession) for item in value):
            raise ValueError("prepared_sessions must be a list of PreparedSession objects.")
        self._prepared_sessions = value
