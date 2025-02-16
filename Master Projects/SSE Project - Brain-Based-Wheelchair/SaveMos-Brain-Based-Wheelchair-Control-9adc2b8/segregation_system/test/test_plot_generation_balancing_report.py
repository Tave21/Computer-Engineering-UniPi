"""
Author: Saverio Mosti
Creation Date: 2024-12-06
Description: Script to test the BalancingReportModel class by generating a dataset of random values
and creating a histogram plot.
"""

from segregation_system.balancing_report_model import BalancingReportModel
from segregation_system.balancing_report_view import BalancingReportView
from segregation_system.segregation_system_parameters import SegregationSystemConfiguration
from segregation_system.test.test_utility_lib import generate_random_prepared_sessions_object_list


def main():
    """
    Main function to test the BalancingReportModel.
    """
    randomized_prepared_sessions = generate_random_prepared_sessions_object_list(100)

    # Count labels manually
    expected_counts = {"move": 0, "turn_left": 0, "turn_right": 0}
    for session in randomized_prepared_sessions:
        expected_counts[session.label] += 1

    # Define a SegregationSystemConfiguration with a tolerance value.
    SegregationSystemConfiguration.configure_parameters("conf/segregation_system_configuration.json")

    # Create the BalancingReportModel.
    report_model = BalancingReportModel(randomized_prepared_sessions)

    # Generate and save the histogram.
    print("Generating the histogram...")
    report_model.generateBalancingReport()
    print("Histogram saved in 'plots/BalancingReport.png'.")

    report_view = BalancingReportView()
    report_view.open_balancing_report() # Open the plot in the default image viewer.


if __name__ == "__main__":
    main()
