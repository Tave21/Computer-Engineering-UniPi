from segregation_system.coverage_report_model import CoverageReportModel
from segregation_system.coverage_report_view import CoverageReportView
from segregation_system.test.test_utility_lib import generate_random_prepared_sessions_object_list


def main():
    """
    Main function to generate the CoverageReportModel with random PreparedSessions
    and generate the radar plot.
    """
    #Generate 100 random prepared sessions
    randomized_prepared_sessions = generate_random_prepared_sessions_object_list(20)

    # Create the CoverageReportModel with the prepared sessions
    coverage_report = CoverageReportModel(randomized_prepared_sessions)
    coverage_report.generateCoverageReport()

    print("Radar bubble plot for CoverageReport generated and saved as 'plots/CoverageReport.png'.")
    cov = CoverageReportView()
    cov.open_coverage_report()

if __name__ == "__main__":
    main()
