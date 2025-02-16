"""
Module: learning_plot
Represents the data used to create the learning plot.

Author: Gabriele Pianigiani

"""
class LearningPlot:
    """This class represents the attributes used to create a learning plot"""
    def __init__(self, error_curve: list):
        """Initialize the learning plot"""
        self._error_curve = error_curve


    def get_learning_error(self):
        """
            Get the learning error curve.
            Returns:
                list: A list of error values corresponding to each iteration
                or epoch of the learning process.
        """
        return self._error_curve