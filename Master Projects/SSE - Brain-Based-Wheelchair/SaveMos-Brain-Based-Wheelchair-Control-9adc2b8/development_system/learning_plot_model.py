"""
Module: learning_plot_model
Manages the creation of the learning report.

Author: Gabriele Pianigiani

"""
from development_system.classifier import Classifier
from development_system.learning_plot import LearningPlot


class LearningPlotModel:
    """Generates the report for the learning plot"""

    def __init__(self):
        """Initialize the learning plot model"""
    @staticmethod
    def generate_learning_report(classifier: Classifier):
        """
            Generates the report for the learning plot
            Parameters:
                classifier (Classifier): classifier object
            Returns:
                learning_report (LearningReport): learning report object
        """
        learning_report = LearningPlot(classifier.get_loss_curve())
        return learning_report
