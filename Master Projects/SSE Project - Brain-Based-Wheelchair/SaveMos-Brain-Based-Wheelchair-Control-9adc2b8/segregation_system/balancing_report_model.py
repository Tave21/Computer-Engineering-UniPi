"""
Author: Saverio Mosti
Creation Date: 2024-12-06
"""
from typing import List
import os
import matplotlib.pyplot as plt

from segregation_system.prepared_session import PreparedSession
from segregation_system.balancing_report import BalancingReport
from segregation_system.segregation_system_parameters import SegregationSystemConfiguration


class BalancingReportModel:
    """
    A model that generates a histogram from a BalancingReport object,
    adds a tolerance line, and saves the plot to a file.

    Attributes:
        balancing_report (BalancingReport): The report to generate the histogram from.
        segregation_config (SegregationSystemConfiguration): The configuration to get the tolerance value.

    Author: Saverio Mosti

    Creation Date: 2024-12-06
    """

    def __init__(self, sessions: List[PreparedSession] ):
        """
        Initializes the BalancingReportModel with the provided balancing report and configuration.

        Args:
            sessions (List[PreparedSession]): The PreparedSessions.
            segregation_config (SegregationSystemConfiguration): The segregation system configuration to access tolerance.
        """

        self.balancing_report = BalancingReport(sessions)

    def generateBalancingReport(self, dir_path = os.path.join('user', 'plots')):
        """
        Generates and saves a histogram for the balancing report, with two tolerance lines at 5%
        above and below the median bar. The histogram is saved as 'BalancingReport.png' in a 'plots' directory.

        This method does the following:
        - Creates a histogram with three bars (Move, Turn Left, Turn Right).
        - Adds two tolerance lines (upper and lower) at 5% above and below the median value.
        - Saves the histogram image in a 'plots' folder as 'BalancingReport.png'.

        Author: Saverio Mosti

        Creation Date: 2024-12-10

        """
        # Extract the data from the balancing report
        data = [self.balancing_report.move, self.balancing_report.turn_left, self.balancing_report.turn_right]
        labels = ['Move', 'Turn Left', 'Turn Right'] # The labels of the plot.

        fig, ax = plt.subplots()

        ax.bar(labels, data, color=['blue', 'green', 'red'])

        # Calculate the median value, where to place the tolerance lines.
        median_value = sorted(data)[1]

        # Calculate the tolerance intervals.
        tolerance = SegregationSystemConfiguration.LOCAL_PARAMETERS['tolerance_interval'] / 100
        lower_tolerance = median_value * (1 - tolerance)
        upper_tolerance = median_value * (1 + tolerance)

        # Add the two tolerance lines to the plot.
        ax.axhline(lower_tolerance, color='orange', linestyle='--', label=f'Lower Tolerance: {lower_tolerance:.2f}')
        ax.axhline(upper_tolerance, color='purple', linestyle='--', label=f'Upper Tolerance: {upper_tolerance:.2f}')

        # Add the labels and the title to the plot.
        ax.set_xlabel('Actions')
        ax.set_ylabel('Frequency')
        ax.set_title('Balancing Report Histogram')

        ax.legend()  # Add a legend to the plot.

        os.makedirs(dir_path, exist_ok=True) # Create the 'plots' directory if it does not exist

        plot_path = os.path.join(dir_path, 'Balancing Report.png')
        plt.savefig(plot_path) # Save the plot as a '.png' image
