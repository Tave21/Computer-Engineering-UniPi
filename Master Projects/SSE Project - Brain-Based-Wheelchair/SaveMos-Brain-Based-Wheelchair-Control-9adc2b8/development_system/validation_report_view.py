""""
Module: validation_report_view
Saves the validation report in a json file.

Author: Gabriele Pianigiani

"""
from development_system.json_validator_reader_and_writer import JsonValidatorReaderAndWriter
from development_system.validation_report import ValidationReport

class ValidationReportView:
    """Shows the validation report"""

    def __init__(self):
        """Initialize the validation report view."""
        self.json_handler = JsonValidatorReaderAndWriter()

    def show_validation_report(self, validation_report: ValidationReport):
        """
            Display and save the validation report.

            Args:
                validation_report (ValidationReport):
                    The validation report object containing the results of the validation process.
        """
        report = {'report': validation_report.get_validation_report(),
                             'overfitting_tolerance': validation_report.get_overfitting_tolerance(),}

        self.json_handler.write_json_file(report, "results/validation_report.json")