""""
Module: validation_report_model
Creates the validation report model.

Author: Gabriele Pianigiani

"""
from typing import List

import joblib

from development_system.classifier import Classifier
from development_system.configuration_parameters import ConfigurationParameters
from development_system.validation_report import ValidationReport


class ValidationReportModel:
    """Generates the report for the validation report"""

    def __init__(self):
        """Initialize the validation report model."""
        self.top_5_classifiers = []

    def generate_validation_report(self, classifiers: List[Classifier]):
        """
            Generate the validation report.
            Returns:
                   validation_report: is the validation report object.
        """
        validation_report = ValidationReport()
        # Assuming at least 5 classifiers
        for i in range(1, 6):

            min_validation_error = -1
            top_classifier = None

            for classifier in classifiers:  # It looks for the classifier with the minimum validation error
                current_validation_error = classifier.get_validation_error()
                if min_validation_error == -1:
                    min_validation_error = current_validation_error
                    top_classifier = classifier
                elif min_validation_error > current_validation_error:
                    min_validation_error = current_validation_error
                    top_classifier = classifier

            #generate classifier report
            classifier_report = {'index': i}
            classifier_report.update(top_classifier.classifier_report())
            self.top_5_classifiers.append(classifier_report)
            joblib.dump(top_classifier, "data/classifier" + str(i) + ".sav")
            classifiers.remove(top_classifier)

        validation_report.set_validation_report(self.top_5_classifiers)
        validation_report.set_overfitting_tolerance(ConfigurationParameters.params['overfitting_tolerance'])
        return validation_report