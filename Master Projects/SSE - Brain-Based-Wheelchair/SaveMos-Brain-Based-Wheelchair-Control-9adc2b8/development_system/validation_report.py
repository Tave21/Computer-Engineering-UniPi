""""
Module: validation_report
Represents the validation report.

Author: Gabriele Pianigiani

"""
class ValidationReport:
    """Class representing the validation report."""

    def __init__(self):
        """Initialize the validation report."""
        self.report = []
        self.overfitting_tolerance = 0.0

    def set_validation_report(self, validation_report: list):
        """Set the validation report.

        Args:
            validation_report (list): The validation report to store.
        """
        self.report = validation_report

    def get_validation_report(self):
        """Get the current validation report.

        Returns:
            list: The current validation report.
        """
        return self.report

    def set_overfitting_tolerance(self, overfitting_tolerance):
        """Set the overfitting tolerance.

        Args:
            overfitting_tolerance: The value of overfitting tolerance to set.
        """
        self.overfitting_tolerance = overfitting_tolerance

    def get_overfitting_tolerance(self):
        """Get the current overfitting tolerance.

        Returns:
            The current value of overfitting tolerance.
        """
        return self.overfitting_tolerance