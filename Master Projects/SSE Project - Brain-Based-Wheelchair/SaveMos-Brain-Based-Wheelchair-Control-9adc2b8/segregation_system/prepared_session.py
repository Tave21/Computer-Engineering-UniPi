"""
Author: Saverio Mosti
Creation Date: 2024-12-06
"""

from typing import List, Tuple


class PreparedSession:
    """
    The `PreparedSession` class represents a prepared session for a data segregation system.

    Author: Saverio Mosti

    Creation Date: 2024-12-06
    """

    def __init__(self, uuid: str, features: List[Tuple[float, float, float, float, str, str]], label: str):
        """
        Initializes a new instance of the `PreparedSession` class.

        Args:
            uuid (str): The unique ID of the session.
            features (List[Tuple[float, float, float, float, str, str]]): A list of features associated with the session.
            label (str): A label associated with the session.
        """
        self._uuid = uuid
        self._features = features
        self._label = label

    def __eq__(self, other: object) -> bool:
        """
        Checks if two PreparedSession instances are equal by comparing their uuid, features, and label.

        Args:
            other (object): The object to compare with.

        Returns:
            bool: True if the objects are equal, False otherwise.
        """
        if not isinstance(other, PreparedSession):
            return False
        return (self._uuid == other._uuid and
                self._features == other._features and
                self._label == other._label)

    # Getter and setter for uuid
    @property
    def uuid(self) -> str:
        """Returns the session ID."""
        return self._uuid

    @uuid.setter
    def uuid(self, value: str):
        """Sets a new value for the session ID."""
        if not isinstance(value, str):
            raise ValueError("uuid must be a string.")
        self._uuid = value

    @property
    def features(self) -> List[Tuple[float, float, float, float, str, str]]:
        """Returns the list of features for the session."""
        return self._features

    @features.setter
    def features(self, value: List[Tuple[float, float, float, float, str, str]]):
        """Sets a new list of features for the session."""
        if not isinstance(value, list):
            raise ValueError("features must be a list.")
        for feature in value:
            if not isinstance(feature, tuple) or len(feature) != 6:
                raise ValueError("Each feature must be a tuple with 4 floats and 2 strings.")
            if not all(isinstance(x, float) for x in feature[:4]):
                raise ValueError("The first four elements of the feature must be floats.")
            if not all(isinstance(x, str) for x in feature[4:]):
                raise ValueError("The last two elements of the feature must be strings.")
        self._features = value

    # Getter and setter for labels
    @property
    def label(self) -> str:
        """Returns the list of labels for the session."""
        return self._label

    @label.setter
    def label(self, value: str):
        """Sets a new list of labels for the session."""
        self._label = value


    @staticmethod
    def from_dictionary(data: dict) -> "PreparedSession":
        """
        Creates a PreparedSession object from a dictionary.

        Args:
            data (dict): A dictionary containing keys `uuid`, `psd_alpha_band`, `psd_beta_band`,
                         `psd_theta_band`, `psd_delta_band`, `activity`, `environment`, and `label`.

        Returns:
            PreparedSession: A new PreparedSession object.

        Raises:
            KeyError: If required keys are missing in the dictionary.
            ValueError: If the data types of values do not match the expected types.
        """
        try:
            uuid = data['uuid']
            alpha = data['psd_alpha_band']
            beta = data['psd_beta_band']
            theta = data['psd_theta_band']
            delta = data['psd_delta_band']
            activity = data['activity']
            environment = data['environment']
            label = data['label']
        except KeyError as e:
            raise KeyError(f"Missing key in input dictionary: {e}")

        # Validate types
        if not isinstance(uuid, str):
            raise ValueError("uuid must be a string.")
        if not all(isinstance(x, (float, int)) for x in [alpha, beta, theta, delta]):
            raise ValueError("PSD bands must be floats or integers.")
        if not isinstance(activity, str):
            raise ValueError("activity must be a string.")
        if not isinstance(environment, str):
            raise ValueError("environment must be a string.")
        if not isinstance(label, str):
            raise ValueError("label must be a string.")

        # Create and return the PreparedSession object
        return PreparedSession(uuid, [alpha, beta, theta, delta, activity, environment], label)

    def to_dictionary(self) -> dict:
        """
        Converts the `PreparedSession` object into a dictionary.

        Returns:
            dict: A dictionary representation of the `PreparedSession` object,
            with keys `sessionID`, `features`, and `label`.
        """
        return  {
            "uuid": self._uuid,
            "label": self._label,
            "psd_alpha_band": self._features[0],
            "psd_beta_band": self._features[1],
            "psd_theta_band": self._features[2],
            "psd_delta_band": self._features[3],
            "activity": self._features[4],
            "environment": self._features[5]
        }

