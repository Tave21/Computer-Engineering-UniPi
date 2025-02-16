import unittest
from unittest.mock import patch, mock_open, MagicMock
import json
from evaluation_system.EvaluationReportModel import EvaluationReportModel
from evaluation_system.Label import Label
from evaluation_system.EvaluationReport import EvaluationReport

class TestEvaluationReportModel(unittest.TestCase):

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.dump")
    @patch("evaluation_system.EvaluationReportModel.EvaluationReport.to_dict")
    def test_create_evaluation_report_success(self, mock_to_dict, mock_json_dump, mock_file):
        """Test successful creation and saving of an evaluation report."""
        # Mock inputs
        classifier_labels = [Label(uuid="1", movements=1, expert=0), Label(uuid="2", movements=0, expert=0)]
        expert_labels = [Label(uuid="1", movements=1, expert=1), Label(uuid="2", movements=1, expert=1)]
        total_errors = 5
        max_consecutive_errors = 2

        # Mock evaluation report to_dict
        mock_to_dict.return_value = {"report": "mocked_report_data"}

        # Create instance of EvaluationReportModel
        report_model = EvaluationReportModel(basedir="..")

        # Call the method
        result = report_model.create_evaluation_report(classifier_labels, expert_labels, total_errors, max_consecutive_errors)

        # Assert results
        self.assertTrue(result)
        self.assertEqual(report_model.evaluation_report_id, 1)  # Ensure ID incremented
        self.assertEqual(mock_file.call_count, 2)  # Two files should be opened
        mock_json_dump.assert_called()  # Ensure data was written to files

    def test_compute_actual_total_errors(self):
        """Test computation of actual total errors."""
        # Mock inputs
        classifier_labels = [Label(uuid="1", movements=1, expert=0), Label(uuid="2", movements=0, expert=0)]
        expert_labels = [Label(uuid="1", movements=1, expert=1), Label(uuid="2", movements=1, expert=1)]

        # Create instance of EvaluationReportModel
        report_model = EvaluationReportModel(basedir="..")

        # Call the method
        actual_total_errors = report_model.compute_actual_total_errors(classifier_labels, expert_labels)

        # Assert results
        self.assertEqual(actual_total_errors, 1)

    def test_compute_actual_max_consecutive_errors(self):
        """Test computation of actual maximum consecutive errors."""
        # Mock inputs
        classifier_labels = [
            Label(uuid="1", movements=1, expert=0),
            Label(uuid="2", movements=0, expert=0),
            Label(uuid="3", movements=0, expert=0),
            Label(uuid="4", movements=1, expert=0),
        ]
        expert_labels = [
            Label(uuid="1", movements=1, expert=1),
            Label(uuid="2", movements=1, expert=1),
            Label(uuid="3", movements=1, expert=1),
            Label(uuid="4", movements=1, expert=1),
        ]

        # Create instance of EvaluationReportModel
        report_model = EvaluationReportModel(basedir="..")

        # Call the method
        actual_max_consecutive_errors = report_model.compute_actual_max_consecutive_errors(classifier_labels, expert_labels)

        # Assert results
        self.assertEqual(actual_max_consecutive_errors, 2)

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.dump")
    def test_create_evaluation_report_failure(self, mock_json_dump, mock_file):
        """Test failure to save the evaluation report due to exception."""
        # Mock inputs
        classifier_labels = [Label(uuid="1", movements=1, expert=0)]
        expert_labels = [Label(uuid="1", movements=1, expert=1)]
        total_errors = 5
        max_consecutive_errors = 2

        # Simulate file write error
        mock_file.side_effect = IOError("Cannot open file")

        # Create instance of EvaluationReportModel
        report_model = EvaluationReportModel(basedir="..")

        # Call the method
        result = report_model.create_evaluation_report(classifier_labels, expert_labels, total_errors, max_consecutive_errors)

        # Assert results
        self.assertFalse(result)

if __name__ == "__main__":
    unittest.main()
