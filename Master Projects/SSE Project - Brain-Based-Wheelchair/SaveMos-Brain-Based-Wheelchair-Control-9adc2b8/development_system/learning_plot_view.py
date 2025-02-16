"""
Module: learning_plot_view
Creates the learning plot.

Author: Gabriele Pianigiani

"""
import matplotlib.pyplot as plt

from development_system.learning_plot import LearningPlot

class LearningPlotView:
    """Shows the learning plot"""

    def __init__(self):
        """Initialize the learning plot view"""

    # Generate a plot with the #iterations on the x-axis and the training error for each iteration on the y-axis
    # It takes as input an object of type LearningError, which stores the training error for each iteration
    @staticmethod
    def show_learning_plot(learning_error: LearningPlot):
        """
            Generates the report for the learning plot
            Parameters:
                learning_error (LearningPlot): the learning plot object used to create the plot
        """
        error_curve = learning_error.get_learning_error()
        plt.plot(range(1, len(error_curve) + 1), error_curve, label="Training error")
        plt.xlabel('# Iterations')
        plt.ylabel('Mean Squared Error (MSE)')
        plt.savefig("plots/learning_plot.png")