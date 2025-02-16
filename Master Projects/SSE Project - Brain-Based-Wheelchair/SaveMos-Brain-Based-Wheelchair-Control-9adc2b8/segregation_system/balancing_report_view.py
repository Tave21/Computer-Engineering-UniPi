"""
Author: Saverio Mosti
Creation Date: 2024-12-06
Description: Class to open the BalancingReport.png image using the default application in Windows.
"""

import os
import subprocess
import platform

class BalancingReportView:
    """
    A view class responsible for opening the 'BalancingReport.png' file
    using the system's default image viewer.

    Author: Saverio Mosti

    Creation Date: 2024-12-06
    """

    def __init__(self):
        """
        Initializes the BalancingReportView object. Prints a confirmation message.
        """
        print("BalancingReportView initialized.")

    def open_balancing_report(self, dir_path = os.path.join('user', 'plots')):
        """
        Opens the 'BalancingReport.png' file using the default image viewer of the system.
        Ensures the file exists before attempting to open it.

        Raises:
            FileNotFoundError: If the 'BalancingReport.png' file does not exist.
            RuntimeError: If the system platform is not supported.
        """
        image_path = os.path.join(dir_path, 'Balancing Report.png')

        # Check if the file exists
        if not os.path.exists(image_path):
            raise FileNotFoundError(f"The file '{image_path}' does not exist.")

        # Determine the platform and open the file with the default viewer
        system_platform = platform.system()
        try:
            if system_platform == "Windows":
                os.startfile(image_path)  # Windows-specific
            elif system_platform == "Darwin":  # macOS
                subprocess.run(["open", image_path], check=True)
            elif system_platform == "Linux":  # Linux systems
                subprocess.run(["xdg-open", image_path], check=True)
            else:
                raise RuntimeError(f"Unsupported platform: {system_platform}")
        except Exception as error:
            print(f"Failed to open the file. Error: {error}")
