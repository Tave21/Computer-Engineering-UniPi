"""
Module: test_validation_report
Tests the creation of the validation report.

Author: Gabriele Pianigiani

"""
from unittest import TestCase

from development_system.validation_orchestrator import ValidationOrchestrator
from development_system.validation_report import ValidationReport
from development_system.configuration_parameters import ConfigurationParameters

class TestValidationReport(TestCase):


    def test_validation_report(self):
        """Test the validity of a ValidationReport object."""
        ConfigurationParameters.load_configuration()
        # Create a validation_report instance
        validation_orchestrator = ValidationOrchestrator()
        validation_report: ValidationReport = validation_orchestrator.validation()

        # Test if 'report' is a list
        assert isinstance(validation_report.get_validation_report(), list), "The report should be a list."

        # Validate each item in the 'report' list
        for classifier in validation_report.get_validation_report():
            assert isinstance(classifier, dict), "Each classifier report should be a dictionary."

            # Check for required keys
            required_keys = [
                'num_iterations', 'validation_error', 'training_error',
                'difference', 'num_layers', 'num_neurons', 'network_complexity'
            ]
            for key in required_keys:
                assert key in classifier, f"Missing key '{key}' in classifier report."

            # Check that values have appropriate types
            assert isinstance(classifier['num_iterations'], int), "'num_iterations' should be an integer."
            assert isinstance(classifier['validation_error'], (int, float)), "'validation_error' should be a number."
            assert isinstance(classifier['training_error'], (int, float)), "'training_error' should be a number."
            assert isinstance(classifier['difference'], (int, float)), "'difference' should be a number."
            assert isinstance(classifier['num_layers'], int), "'num_layers' should be an integer."
            assert isinstance(classifier['num_neurons'], int), "'num_neurons' should be an integer."
            assert isinstance(classifier['network_complexity'], int), "'network_complexity' should be an integer."

        # Test if 'overfitting_tolerance' is set
        assert validation_report.get_overfitting_tolerance() is not None, "The overfitting tolerance should not be None."

        # Check that 'overfitting_tolerance' has an appropriate type
        assert isinstance(validation_report.get_overfitting_tolerance(),
                          (int, float)), "'overfitting_tolerance' should be a number."

        print("The object is a correct validation report")