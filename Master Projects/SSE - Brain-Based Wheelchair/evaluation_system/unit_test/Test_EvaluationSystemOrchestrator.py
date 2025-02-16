import unittest
from unittest.mock import patch, MagicMock, mock_open
import json
from evaluation_system.EvaluationSystemOrchestrator import EvaluationSystemOrchestrator
from evaluation_system.LabelsBuffer import LabelsBuffer
from evaluation_system.LabelReceiver_and_ConfigurationSender import LabelReceiver_and_ConfigurationSender
from evaluation_system.EvaluationReportModel import EvaluationReportModel
from evaluation_system.EvaluationSystemParameters import EvaluationSystemParameters
from evaluation_system.Label import Label
import os

class TestEvaluationSystemOrchestrator(unittest.TestCase):

    @classmethod
    def tearDownClass(cls):
        """Clean up the test database."""
        try:
            # Ensure all database connections are closed
            import gc  # Garbage collector to force cleanup of SQLite objects
            gc.collect()  # Ensure all SQLite objects are released

            if os.path.exists("labels.db"):
                os.remove("labels.db")
        except PermissionError as e:
            print(f"PermissionError: {e}. Make sure no process is using the file.")

    @patch("evaluation_system.EvaluationSystemParameters.EvaluationSystemParameters.loadParameters", return_value=None)
    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    @patch("jsonschema.validate")
    def test_get_classifier_evaluation_success(self, mock_validate, mock_json_load, mock_file, mock_load_params):
        """Test successful retrieval and validation of classifier evaluation."""
        mock_json_load.side_effect = [
            {"classifier_evaluation": "waiting_for_evaluation"},  # JSON content
            {"type": "object", "properties": {"classifier_evaluation": {"type": "string"}}},  # JSON schema
        ]

        orchestrator = EvaluationSystemOrchestrator(basedir="..")

        exists, data = orchestrator._get_classifier_evaluation()

        self.assertTrue(exists)
        self.assertEqual(data["classifier_evaluation"], "waiting_for_evaluation")
        mock_validate.assert_called_once()

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    @patch("jsonschema.validate")
    def test_get_classifier_evaluation_failure(self, mock_validate, mock_json_load, mock_file):
        """Test failure when classifier evaluation file is missing or invalid."""
        mock_file.side_effect = FileNotFoundError()

        orchestrator = EvaluationSystemOrchestrator(basedir="..")

        exists, data = orchestrator._get_classifier_evaluation()

        self.assertFalse(exists)
        self.assertIsNone(data)
        mock_validate.assert_not_called()

    @patch("evaluation_system.EvaluationSystemOrchestrator.LabelReceiver_and_ConfigurationSender")
    @patch("evaluation_system.EvaluationSystemOrchestrator.LabelsBuffer")
    @patch("evaluation_system.EvaluationSystemOrchestrator.EvaluationReportModel")
    @patch("os.remove")
    def test_evaluate_creates_evaluation_report(self, mock_os_remove, mock_report_model,
                                                mock_labels_buffer, mock_receiver):
        """Test Evaluate method creates evaluation report correctly."""
        mock_receiver_instance = mock_receiver.return_value
        mock_labels_buffer_instance = mock_labels_buffer.return_value
        mock_report_model_instance = mock_report_model.return_value

        EvaluationSystemParameters.loadParameters("..")

        # Mock the labels buffer to return a sufficient number of labels
        mock_labels_buffer_instance.get_num_classifier_labels.return_value = EvaluationSystemParameters.LOCAL_PARAMETERS["minimum_number_labels"]
        mock_labels_buffer_instance.get_num_expert_labels.return_value = EvaluationSystemParameters.LOCAL_PARAMETERS["minimum_number_labels"]

        # Mock the labels returned by the labels buffer
        mock_labels_buffer_instance.get_classifier_labels.return_value = [
            {"uuid": "0", "movements": 0, "expert": False}]
        mock_labels_buffer_instance.get_expert_labels.return_value = [{"uuid": "0", "movements": 0, "expert": True}]

        orchestrator = EvaluationSystemOrchestrator(basedir="..")
        orchestrator.service = True

        orchestrator.Evaluate()

        # Check that the evaluation report was created
        mock_report_model_instance.create_evaluation_report.assert_called_once_with(
            [{"uuid": "0", "movements": 0, "expert": False}],
            [{"uuid": "0", "movements": 0, "expert": True}],
            EvaluationSystemParameters.LOCAL_PARAMETERS["total_errors"],
            EvaluationSystemParameters.LOCAL_PARAMETERS["max_consecutive_errors"]
        )

        # Check that the labels were deleted
        mock_labels_buffer_instance.delete_labels.assert_called_once_with(
            EvaluationSystemParameters.LOCAL_PARAMETERS["minimum_number_labels"])


    @patch("os.remove")
    @patch("evaluation_system.EvaluationSystemOrchestrator.EvaluationSystemParameters.loadParameters")
    @patch("evaluation_system.EvaluationSystemOrchestrator.LabelReceiver_and_ConfigurationSender")
    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    @patch("jsonschema.validate")
    def test_evaluate_handles_human_operator_good_evaluation(self, mock_validate, mock_json_load, mock_file, mock_receiver, mock_load_params, mock_os_remove):
        """Test the evaluation process when Human Operator evaluates classifier as 'good'."""
        mock_json_load.side_effect = [
            {"classifier_evaluation": "good"},  # JSON content
            {"type": "object", "properties": {"classifier_evaluation": {"type": "string"}}},  # JSON schema
        ]

        mock_load_params.return_value = None
        mock_receiver_instance = mock_receiver.return_value

        orchestrator = EvaluationSystemOrchestrator(basedir="..")
        orchestrator.service = True

        orchestrator.Evaluate()

        mock_os_remove.assert_called_once_with("../human_operator_workspace/classifier_evaluation.json")
        mock_receiver_instance.send_timestamp.assert_called_once()

    @patch("os.remove")
    @patch("evaluation_system.EvaluationSystemOrchestrator.EvaluationSystemParameters.loadParameters")
    @patch("evaluation_system.EvaluationSystemOrchestrator.LabelReceiver_and_ConfigurationSender")
    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    @patch("jsonschema.validate")
    def test_evaluate_handles_human_operator_bad_evaluation(self, mock_validate, mock_json_load, mock_file, mock_receiver, mock_load_params, mock_os_remove):
        """Test the evaluation process when Human Operator evaluates classifier as 'bad'."""
        mock_json_load.side_effect = [
            {"classifier_evaluation": "bad"},  # JSON content
            {"type": "object", "properties": {"classifier_evaluation": {"type": "string"}}},  # JSON schema
        ]

        mock_load_params.return_value = None
        mock_receiver_instance = mock_receiver.return_value

        orchestrator = EvaluationSystemOrchestrator(basedir="..")
        orchestrator.service = True

        orchestrator.Evaluate()

        mock_os_remove.assert_called_once_with("../human_operator_workspace/classifier_evaluation.json")
        mock_receiver_instance.send_timestamp.assert_called()
        mock_receiver_instance.send_configuration.assert_called_once()

if __name__ == "__main__":
    unittest.main()
