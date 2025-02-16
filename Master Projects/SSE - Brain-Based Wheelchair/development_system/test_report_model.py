"""
Module: test_report_model
Creates the test report.

Author: Gabriele Pianigiani

"""
from development_system.classifier import Classifier
from development_system.configuration_parameters import ConfigurationParameters
from development_system.test_report import TestReport


class TestReportModel:
    """Generates the report for the test"""

    def __init__(self):
        """Initialize the test report model."""

    @staticmethod
    def generate_test_report(classifier: Classifier):
        """
            Generates a test report for the given classifier.

            Args:
                classifier (Classifier): An instance of a classifier that provides
                validation and test error metrics.

            Returns:
                TestReport: An instance of `TestReport` containing the generalization
                tolerance, validation error, test error, and the difference between
                validation and test errors.
        """

        test_report = TestReport()
        test_report.set_generalization_tolerance(ConfigurationParameters.params['generalization_tolerance'])
        test_report.set_validation_error(classifier.get_validation_error())
        test_report.set_test_error(classifier.get_test_error())
        test_report.set_difference(classifier.get_valid_test_error_difference())
        return test_report