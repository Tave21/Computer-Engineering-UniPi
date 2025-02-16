"""
Module: test_report
Represents the test report generated after the testing phase.

Author: Gabriele Pianigiani

"""
class TestReport:
    """Class representing the test report."""

    def __init__(self):
        """Initialize the test report."""
        self.generalization_tolerance = 0.0
        self.validation_error = 0.0
        self.test_error = 0.0
        self.difference = 0.0

    # Getter and Setter for generalization_tolerance
    def set_generalization_tolerance(self, generalization_tolerance):
        """
        Sets the generalization tolerance.

        Args:
            generalization_tolerance (float): The tolerance level for generalization,
            which determines the acceptable level of error in the generalization phase.
        """
        self.generalization_tolerance = generalization_tolerance

    def get_generalization_tolerance(self):
        """
            Retrieves the generalization tolerance.

            Returns:
                float: The currently set generalization tolerance.
        """
        return self.generalization_tolerance

    # Getter and Setter for validation_error
    def set_validation_error(self, validation_error):
        """
            Sets the validation error.

            Args:
                validation_error (float): The error measured on the validation dataset.
        """
        self.validation_error = validation_error

    def get_validation_error(self):
        """
            Retrieves the validation error.

            Returns:
                float: The error value measured on the validation dataset.
        """
        return self.validation_error

    # Getter and Setter for test_error
    def set_test_error(self, test_error):
        """
            Sets the test error.

            Args:
                test_error (float): The error measured on the test dataset.
        """
        self.test_error = test_error

    def get_test_error(self):
        """
            Retrieves the test error.

            Returns:
                float: The error value measured on the test dataset.
        """
        return self.test_error

    # Getter and Setter for difference
    def set_difference(self, difference):
        """
        Sets the difference value.

        Args:
            difference (float): A calculated difference, between test and validation error.
        """
        self.difference = difference

    def get_difference(self):
        """
            Retrieves the difference value.

            Returns:
                float: The currently set difference value.
        """
        return self.difference