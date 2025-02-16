"""
Author: Alessandro Ascani
"""

class Label:
    """
    Data Object class to represent a Label created in Production System.
    """

    def __init__(self, uuid: str, movements: str):
        """
        Initialize the Label with uuid and movements fields.

        """
        self._uuid = uuid
        self._movements = movements

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
    def movements(self) -> str:
        """
        Get the movements label.

        Returns:
            int: The movements label.
        """
        return self._movements

    @movements.setter
    def movements(self, value: str):
        """
        Set the movements label.

        Args:
            value (int): The new movements label.
        """
        self._movements = value

    def convert_movement(self):
        """
        Mapping int value of movement in string

        Returns: string value

        """
        if self._movements == 0:
            return "turnRight"
        elif self._movements == 1:
            return "turnLeft"
        else:
            return "move"

    def to_dictionary(self) -> dict:
        """
        convert a label object into dictionary

        Returns:

            dict: dictionary that content label field

        """
        #movement = self.convert_movement()
        result = {
            "uuid": self._uuid,
            "movements": self._movements
        }

        return result
