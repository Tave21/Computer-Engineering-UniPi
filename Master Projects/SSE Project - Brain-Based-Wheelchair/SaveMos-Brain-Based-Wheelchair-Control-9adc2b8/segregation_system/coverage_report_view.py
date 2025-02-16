"""
Author: Saverio Mosti
Creation Date: 2024-12-06
Description: Class to open the CoverageReport.png image using the default application in Windows.
"""

import os
import subprocess
import platform

class CoverageReportView:
    """
    A view class responsible for opening the 'CoverageReport.png' file
    using the system's default image viewer.
    """

    def __init__(self):
        """
        Initializes the CoverageCoverageReportView object. Prints a confirmation message.
        """
        print("CoverageReportView initialized.")

    def open_coverage_report(self, dir_path = os.path.join('user', 'plots')):
        """
        Opens the 'CoverageReport.png' file using the default image viewer of the system.
        Ensures the file exists before attempting to open it.

        Raises:
            FileNotFoundError: If the 'CoverageReport.png' file does not exist.
            RuntimeError: If the system platform is not supported.
        """
        image_path = os.path.join(dir_path, 'Input Coverage Report.png')


        # Check if the file exists
        if not os.path.exists(image_path):
            print(f"'CoverageReport.png' file does not exist.")

        # Determine the platform and open the file with the default viewer
        system_platform = platform.system()
        try:
            if system_platform == "Windows":
                os.startfile(image_path)  # Windows-specific.
            elif system_platform == "Darwin":  # macOS
                subprocess.run(["open", image_path], check=True)
            elif system_platform == "Linux":  # Linux systems
                subprocess.run(["xdg-open", image_path], check=True)
            else:
                raise RuntimeError(f"Unsupported platform: {system_platform}")
        except Exception as error:
            print(f"Failed to open the file. Error: {error}")
