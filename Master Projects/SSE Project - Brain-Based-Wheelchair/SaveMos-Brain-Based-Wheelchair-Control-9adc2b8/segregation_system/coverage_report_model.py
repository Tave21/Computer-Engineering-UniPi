import os
from typing import List

import matplotlib.pyplot as plt
import numpy as np

from segregation_system.prepared_session import PreparedSession

class CoverageReportModel:
    """
    This class generates a radar bubble plot (polar plot) based on an array of PreparedSession objects.
    It saves the plot in a 'plots' directory as 'CoverageReport.png'.

    Author: Saverio Mosti
    Creation Date: 2024-12-06
    """

    def __init__(self, prepared_sessions: List[PreparedSession]):
        """
        Initializes the CoverageReportModel with the provided prepared sessions.
        :param prepared_sessions: List of PreparedSession objects.
        """
        self.prepared_sessions = prepared_sessions
        self.features_names = [
            "PSD Alpha Band", "PSD Beta Band", "PSD Delta Band", "PSD Tetha Band",
            "Activity + Scatter", "Environment + Scatter"
        ]

    def generateCoverageReport(self, dir_path=os.path.join('user', 'plots')):
        """
        Creates a radar bubble plot for the provided prepared sessions.
        Save the plot to the "plots" directory as 'CoverageReport.png'.
        """
        num_features = len(self.features_names)  # Get the number of features.

        # Create an array of angles for each feature (equally spaced in a circle).
        angles = np.linspace(0, 2 * np.pi, num_features, endpoint=False).tolist()

        # Add the first angle to the end to close the circle.
        angles += angles[:1]

        # Create the plot.
        fig, ax = plt.subplots(figsize=(6, 6), subplot_kw=dict(polar=True))

        # Mapping for environment and activity strings to numeric values.
        environment_mapping = {
            "slippery": 0.20,
            "plain": 0.40,
            "slope": 0.60,
            "track": 0.80,
            "house": 1.00,
        }

        activity_mapping = {
            "shopping": 0.20,
            "sport": 0.40,
            "cooking": 0.60,
            "relax": 0.80,
            "gaming": 1.00,
        }

        # Initialize density array (assuming predefined number of features and bins)
        density = np.zeros((len(self.prepared_sessions[0].features), 10))

        # Define the range of features for normalization
        feature_min, feature_max = -27.30245, 28.170070 # This is the 98° percentile.

        # Function to normalize feature values to [0, 1]
        def normalize(value):
            return (value - feature_min) / (feature_max - feature_min)

        # First loop to calculate the density
        for session in self.prepared_sessions:
            for i, value in enumerate(session.features):
                # Convert value to numeric if necessary
                if isinstance(value, (int, float)):
                    radius = normalize(float(value))
                elif value in environment_mapping:
                    radius = environment_mapping[value]
                elif value in activity_mapping:
                    radius = activity_mapping[value]
                else:
                    raise ValueError(f"Unrecognized feature value: {value}")

                # Calculate the radial bin for density
                radial_bin = int(np.clip(radius * 10, 0, 9))  # 10 bins (0-1 range)

                # Increment the density for this feature and bin
                density[i, radial_bin] += 1

        # Normalize density to [0, 1]
        density = density / np.max(density)

        # Second loop to plot the radar bubbles
        for session in self.prepared_sessions:
            for i, value in enumerate(session.features):
                # Convert value to numeric if necessary
                if isinstance(value, (int, float)):
                    radius = normalize(float(value))
                elif value in environment_mapping:
                    radius = environment_mapping[value]
                elif value in activity_mapping:
                    radius = activity_mapping[value]
                else:
                    raise ValueError(f"Unrecognized feature value: {value}")

                # Angular position (phase)
                angle = angles[i]

                # Retrieve the bin relative to the current bubble
                radial_bin = int(np.clip(radius * 10, 0, 9))

                # Scale bubble size based on density
                bubble_size = density[i, radial_bin] * 100  # Adjust scale as needed

                # Plot the bubble
                ax.scatter(angle, radius, s=bubble_size, color="lightblue", alpha=0.6)

        # Add concentric rings with labels
        ax.set_rmax(1)  # Set the maximum radius (1 is the default)
        ax.set_rticks([0.2, 0.4, 0.6, 0.8, 1.0])  # Set radial ticks
        ax.set_rlabel_position(40)  # Position the radial labels on the outermost ring
        ax.tick_params(labelsize=8)  # Set the size of the radial labels

        # Enable angular ticks and set their positions at the appropriate angles
        ax.set_xticks(angles[:-1])  # Enable angular ticks at the feature angles
        ax.set_xticklabels(self.features_names, fontsize=8, color='black', weight='bold')  # Labels for the features

        ax.set_title("Coverage Report", size=16, color='black', y=1.1)  # Add the title to the plot.

        # Save the plot to the "plots" directory as 'CoverageReport.png'.
        if not os.path.exists(dir_path):
            os.makedirs(dir_path)  # If the directory does not exist, it will be created.

        plot_path = os.path.join(dir_path, 'Input Coverage Report.png')
        plt.savefig(plot_path)  # Save the plot as a '.png' image
