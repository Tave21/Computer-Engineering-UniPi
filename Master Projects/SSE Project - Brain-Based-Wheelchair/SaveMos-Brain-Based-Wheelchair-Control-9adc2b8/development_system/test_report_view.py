"""
Module: test_report_view
Writes the test report in a json file.

Author: Gabriele Pianigiani

"""
from development_system.configuration_parameters import ConfigurationParameters
from development_system.json_validator_reader_and_writer import JsonValidatorReaderAndWriter
from development_system.test_report import TestReport


class TestReportView:
    """Shows the test report"""

    def __init__(self):
        """Initialize the test report view."""
        self.json_handler = JsonValidatorReaderAndWriter()

    def show_test_report(self, test_report: TestReport):
        """
        Displays and saves the test report.

        Args:
            test_report (TestReport): An instance of `TestReport` containing the
            metrics to be displayed and saved.

        Returns:
            None
        """
        report = {'generalization_tolerance': ConfigurationParameters.params['generalization_tolerance'],
                'validation_error': test_report.get_validation_error(),
                'test_error': test_report.get_test_error(),
                'difference': test_report.get_difference(),}

        self.json_handler.write_json_file(report, "results/test_report.json")