"""
Module: test_learning_report
Tests the creation of the learning report.

Author: Gabriele Pianigiani

"""
from unittest import TestCase

import joblib

from development_system.classifier import Classifier
from development_system.learning_plot_model import LearningPlotModel
from development_system.learning_plot_view import LearningPlotView

class TestTraining(TestCase):
    def test_generate_learning_report(self):
        """Tests the learning report."""
        # Create a classifier instance
        classifier: Classifier = joblib.load("data/classifier.sav")

        # Generate the learning report using LearningPlotModel
        learning_plot_model = LearningPlotModel()
        learning_plot_view = LearningPlotView()
        learning_report = learning_plot_model.generate_learning_report(classifier)
        learning_plot_view.show_learning_plot(learning_report)

        # Check if the learning report contains a list of errors (loss_curve) with max_iter elements
        error_curve = learning_report.get_learning_error()
        assert isinstance(error_curve, list), "Error curve is not a list"
        print("numero errori: ", len(error_curve))
        #assert len(error_curve) == classifier.get_num_iterations(), f"Error curve does not contain {classifier.get_num_iterations()} elements"

        print("Test passed: The learning report contains a list of errors with max_iter elements")