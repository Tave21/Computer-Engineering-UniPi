"""
Author: Giovanni Ligato
"""

class Label:
    """
    Data Object class to represent a Label used inside the Evaluation System.
    """

    def __init__(self, uuid: str, movements: int, expert: bool):
        """
        Initialize the Label with uuid, movements, and expert fields.

        Args:
            uuid (str): The unique identifier for the label.
            movements (int): Integer label associated to the movements.
            expert (bool): Whether the label is from an expert or from the classifier.
        """
        self._uuid = uuid
        self._movements = movements
        self._expert = expert

    @property
    def uuid(self) -> str:
        """
        Get the uuid of the label.

        Returns:
            str: The uuid of the label.
        """
        return self._uuid

    @uuid.setter
    def uuid(self, value: str):
        """
        Set the uuid of the label.

        Args:
            value (str): The new uuid of the label.
        """
        self._uuid = value

    @property
    def movements(self) -> int:
        """
        Get the movements label.

        Returns:
            int: The movements label.
        """
        return self._movements

    @movements.setter
    def movements(self, value: int):
        """
        Set the movements label.

        Args:
            value (int): The new movements label.
        """
        self._movements = value

    @property
    def expert(self) -> bool:
        """
        Get the expert status of the label.

        Returns:
            bool: The expert status of the label. True if the label is from an expert, False otherwise.
        """
        return self._expert

    @expert.setter
    def expert(self, value: bool):
        """
        Set the expert status of the label.

        Args:
            value (bool): The new expert status of the label. True if the label is from an expert, False otherwise.
        """
        self._expert = value

    def to_dict(self) -> dict:
        """
        Convert the Label object to a dictionary.

        Returns:
            dict: A dictionary representing the Label object.
        """
        return {
            "uuid": self.uuid,
            "movements": self.movements,
            "expert": self.expert
        }