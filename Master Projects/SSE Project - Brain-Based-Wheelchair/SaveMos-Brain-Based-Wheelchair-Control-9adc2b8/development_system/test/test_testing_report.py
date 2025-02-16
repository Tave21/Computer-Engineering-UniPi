"""
Module: test_testing_report
Tests the creation of the testing report.

Author: Gabriele Pianigiani

"""
from unittest import TestCase

from development_system.test_report import TestReport
from development_system.testing_orchestrator import TestingOrchestrator
from development_system.configuration_parameters import ConfigurationParameters

class TestTestingReport(TestCase):

    def test_testing_report(self):
        """Test the validity of a TestReport object."""
        ConfigurationParameters.load_configuration()
        # Create a validation_report instance
        testing_orchestrator = TestingOrchestrator()
        test_report: TestReport = testing_orchestrator.test()

        # Validate 'generalization_tolerance'
        generalization_tolerance = test_report.get_generalization_tolerance()
        assert generalization_tolerance is not None, "The generalization tolerance should not be None."
        assert isinstance(generalization_tolerance, (int, float)), "'generalization_tolerance' should be a number."

        # Validate 'validation_error'
        validation_error = test_report.get_validation_error()
        assert validation_error is not None, "The validation error should not be None."
        assert isinstance(validation_error, (int, float)), "'validation_error' should be a number."

        # Validate 'test_error'
        test_error = test_report.get_test_error()
        assert test_error is not None, "The test error should not be None."
        assert isinstance(test_error, (int, float)), "'test_error' should be a number."

        # Validate 'difference'
        difference = test_report.get_difference()
        assert difference is not None, "The difference should not be None."
        assert isinstance(difference, (int, float)), "'difference' should be a number."

        print("difference= ", difference)
        print("error difference= ", ((test_error - validation_error)/test_error))
        # Additional logical checks (optional)
        assert difference == ((test_error - validation_error)/test_error), (
            "'difference' should be the absolute difference between 'validation_error' and 'test_error'."
        )

        print("The object is a correct validation report")