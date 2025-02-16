"""
Author: Giovanni Ligato
"""

import os
import json
from evaluation_system.Label import Label
from evaluation_system.EvaluationReport import EvaluationReport

class EvaluationReportModel:
    """
    Creates and saves to a json file the evaluation report of the Evaluation System.
    """

    def __init__(self, basedir: str = "."):
        """
        Initialize the EvaluationReportModel with an evaluation report id.

        :param basedir: Base directory of the Evaluation System.
        """
        self.basedir = basedir

        self.evaluation_report_id = self._get_next_evaluation_report_id()


    def _get_next_evaluation_report_id(self) -> int:
        """
        Get the next evaluation report id by checking the reports folder for existing report files.

        :return: The next evaluation report id.
        """

        if not os.path.exists(f"{self.basedir}/reports"):
            return 0

        report_files = [f for f in os.listdir(f"{self.basedir}/reports") if
                        f.startswith("evaluation_report_") and f.endswith(".json")]
        if not report_files:
            return 0
        max_id = max(int(f.split("_")[-1].split(".")[0]) for f in report_files)
        return max_id + 1


    def create_evaluation_report(self, classifier_labels: list[Label], expert_labels: list[Label],
                                 total_errors: int, max_consecutive_errors: int) -> bool:
        """
        Create an evaluation report with the given classifier and expert labels and save it to a json file.

        :param classifier_labels: List of classifier labels.
        :param expert_labels: List of expert labels.
        :param total_errors: Total errors allowed.
        :param max_consecutive_errors: Maximum consecutive errors allowed.
        :return: True if the evaluation report was successfully saved, False otherwise.

        """

        actual_total_errors = self.compute_actual_total_errors(classifier_labels, expert_labels)
        actual_max_consecutive_errors = self.compute_actual_max_consecutive_errors(classifier_labels, expert_labels)

        evaluation_report = EvaluationReport(classifier_labels, expert_labels,
                                             total_errors, max_consecutive_errors,
                                             actual_total_errors, actual_max_consecutive_errors)

        try:
            with open(f"{self.basedir}/reports/evaluation_report_{self.evaluation_report_id}.json", "w") as f:
                data = evaluation_report.to_dict()
                json.dump(data, f, ensure_ascii=False, indent=4)
                self.evaluation_report_id += 1

                # Create the classifier_evaluation.json file where the Human Operator will write the evaluation.
                with open(f"{self.basedir}/human_operator_workspace/classifier_evaluation.json", "w") as ce:
                    json.dump({"classifier_evaluation": "waiting_for_evaluation"}, ce, ensure_ascii=False, indent=4)

                return True
        except Exception as e:
            print(f"Error saving evaluation report: {e}")
            return False


    def compute_actual_total_errors(self, classifier_labels: list[Label], expert_labels: list[Label]) -> int:
        """
        Compute the actual total errors in the evaluation report.

        :param classifier_labels: List of classifier labels.
        :param expert_labels: List of expert labels.
        :return: The actual total errors.
        """

        actual_total_errors = 0
        for i in range(min(len(classifier_labels), len(expert_labels))):
            if classifier_labels[i].movements != expert_labels[i].movements:
                actual_total_errors += 1
        return actual_total_errors


    def compute_actual_max_consecutive_errors(self, classifier_labels: list[Label], expert_labels: list[Label]) -> int:
        """
        Compute the actual maximum consecutive errors in the evaluation report.

        :param classifier_labels: List of classifier labels.
        :param expert_labels: List of expert labels.
        :return: The actual maximum consecutive errors.

        """

        actual_max_consecutive_errors = 0
        consecutive_errors = 0
        for i in range(min(len(classifier_labels), len(expert_labels))):
            if classifier_labels[i].movements != expert_labels[i].movements:
                consecutive_errors += 1
                if consecutive_errors > actual_max_consecutive_errors:
                    actual_max_consecutive_errors = consecutive_errors
            else:
                consecutive_errors = 0
        return actual_max_consecutive_errors