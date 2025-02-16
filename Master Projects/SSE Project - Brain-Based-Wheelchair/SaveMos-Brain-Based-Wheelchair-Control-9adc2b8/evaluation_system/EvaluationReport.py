"""
Author: Giovanni Ligato
"""

from evaluation_system.Label import Label

class EvaluationReport:
    """
    Model class for the Evaluation Report
    """

    def __init__(self, classifier_labels: list[Label], expert_labels: list[Label],
                 total_errors: int, max_consecutive_errors: int,
                 actual_total_errors: int, actual_max_consecutive_errors: int):
        """
        Constructor for the EvaluationReport class.

        :param classifier_labels: List of classifier labels.
        :param expert_labels: List of expert labels.
        :param total_errors: Total errors allowed.
        :param max_consecutive_errors: Maximum consecutive errors allowed.
        :param actual_total_errors: Actual total errors.
        :param actual_max_consecutive_errors: Actual maximum consecutive errors.

        """
        self.classifier_labels = classifier_labels
        self.expert_labels = expert_labels
        self.total_errors = total_errors
        self.max_consecutive_errors = max_consecutive_errors
        self.actual_total_errors = actual_total_errors
        self.actual_max_consecutive_errors = actual_max_consecutive_errors

    def get_classifier_labels(self) -> list[Label]:
        """
        Get the classifier labels.

        :return: The classifier labels.
        """
        return self.classifier_labels

    def get_expert_labels(self) -> list[Label]:
        """
        Get the expert labels.

        :return: The expert labels.
        """
        return self.expert_labels

    def get_total_errors(self) -> int:
        """
        Get the total errors allowed.

        :return: The total errors allowed.
        """
        return self.total_errors

    def get_max_consecutive_errors(self) -> int:
        """
        Get the maximum consecutive errors allowed.

        :return: The maximum consecutive errors allowed.
        """
        return self.max_consecutive_errors

    def get_actual_total_errors(self) -> int:
        """
        Get the actual total errors.

        :return: The actual total errors.
        """
        return self.actual_total_errors

    def get_actual_max_consecutive_errors(self) -> int:
        """
        Get the actual maximum consecutive errors.

        :return: The actual maximum consecutive errors.
        """
        return self.actual_max_consecutive_errors

    def to_dict(self) -> dict:
        """
        Convert the EvaluationReport object to a dictionary.

        :return: A dictionary representing the EvaluationReport object.
        """
        return {
            "classifier_labels": [label.to_dict() for label in self.classifier_labels],
            "expert_labels": [label.to_dict() for label in self.expert_labels],
            "total_errors": self.total_errors,
            "max_consecutive_errors": self.max_consecutive_errors,
            "actual_total_errors": self.actual_total_errors,
            "actual_max_consecutive_errors": self.actual_max_consecutive_errors
        }
