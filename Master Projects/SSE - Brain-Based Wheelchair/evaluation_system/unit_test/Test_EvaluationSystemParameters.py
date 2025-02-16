import unittest
from unittest.mock import patch, mock_open
import jsonschema
import json
from evaluation_system.EvaluationSystemParameters import EvaluationSystemParameters

class TestEvaluationSystemParameters(unittest.TestCase):

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    @patch("evaluation_system.EvaluationSystemParameters.EvaluationSystemParameters._validate_json")
    def test_load_parameters_success(self, mock_validate_json, mock_json_load, mock_file):
        """Test successful loading of parameters."""
        # Mock valid JSON data for local and global parameters
        local_parameters = {
            "minimum_number_labels": 10,
            "total_errors": 5,
            "max_consecutive_errors": 2,
            "service": True
        }
        global_parameters = {
            "Evaluation System": {"port": 5006},
            "Ingestion System": {"ip": "127.0.0.1"},
            "Production System": {"ip": "127.0.0.2"},
            "Messaging System": {"ip": "127.0.0.3", "port": 5000},
            "Service Class": {"ip": "127.0.0.4", "port": 5001}
        }

        # Setup mock behaviors
        mock_json_load.side_effect = [local_parameters, global_parameters]
        mock_validate_json.return_value = True

        # Call the method
        EvaluationSystemParameters.loadParameters(basedir="..")

        # Assert local parameters
        self.assertEqual(EvaluationSystemParameters.LOCAL_PARAMETERS["minimum_number_labels"], 10)
        self.assertEqual(EvaluationSystemParameters.LOCAL_PARAMETERS["total_errors"], 5)
        self.assertEqual(EvaluationSystemParameters.LOCAL_PARAMETERS["max_consecutive_errors"], 2)
        self.assertTrue(EvaluationSystemParameters.LOCAL_PARAMETERS["service"])

        # Assert global parameters
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Evaluation System"]["port"], 5006)
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Ingestion System"]["ip"], "127.0.0.1")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Production System"]["ip"], "127.0.0.2")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Messaging System"]["ip"], "127.0.0.3")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Messaging System"]["port"], 5000)
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Service Class"]["ip"], "127.0.0.4")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Service Class"]["port"], 5001)

        # Assert file open calls
        self.assertEqual(mock_file.call_count, 2)

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    @patch("evaluation_system.EvaluationSystemParameters.EvaluationSystemParameters._validate_json")
    def test_load_parameters_invalid_local(self, mock_validate_json, mock_json_load, mock_file):
        """Test loading parameters with invalid local parameters."""
        # Mock invalid local JSON data
        local_parameters = {}
        global_parameters = {
            "Evaluation System": {"port": 5006},
            "Ingestion System": {"ip": "127.0.0.1"},
            "Production System": {"ip": "127.0.0.2"},
            "Messaging System": {"ip": "127.0.0.3", "port": 5000},
            "Service Class": {"ip": "127.0.0.4", "port": 5001}
        }

        # Setup mock behaviors
        mock_json_load.side_effect = [local_parameters, global_parameters]
        mock_validate_json.side_effect = [False, True]

        # Call the method
        EvaluationSystemParameters.loadParameters(basedir="..")

        # Assert LOCAL_PARAMETERS is an empty dictionary
        self.assertEqual(EvaluationSystemParameters.LOCAL_PARAMETERS, {})

        # Assert global parameters are loaded
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Evaluation System"]["port"], 5006)
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Ingestion System"]["ip"], "127.0.0.1")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Production System"]["ip"], "127.0.0.2")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Messaging System"]["ip"], "127.0.0.3")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Messaging System"]["port"], 5000)
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Service Class"]["ip"], "127.0.0.4")
        self.assertEqual(EvaluationSystemParameters.GLOBAL_PARAMETERS["Service Class"]["port"], 5001)

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    def test_validate_json_valid(self, mock_json_load, mock_file):
        """Test validation of valid JSON data."""
        schema = {"type": "object", "properties": {"key": {"type": "string"}}, "required": ["key"]}
        valid_json = {"key": "value"}

        mock_file.return_value.read.return_value = json.dumps(schema)
        with patch("jsonschema.validate", return_value=None):
            result = EvaluationSystemParameters._validate_json(valid_json, "local", basedir="..")
            self.assertTrue(result)

    @patch("builtins.open", new_callable=mock_open)
    @patch("json.load")
    def test_validate_json_invalid(self, mock_json_load, mock_file):
        """Test validation of invalid JSON data."""
        schema = {"type": "object", "properties": {"key": {"type": "string"}}, "required": ["key"]}
        invalid_json = {}

        mock_file.return_value.read.return_value = json.dumps(schema)
        with patch("jsonschema.validate", side_effect=jsonschema.ValidationError("Invalid JSON")):
            result = EvaluationSystemParameters._validate_json(invalid_json, "local", basedir="..")
            self.assertFalse(result)

if __name__ == "__main__":
    unittest.main()
