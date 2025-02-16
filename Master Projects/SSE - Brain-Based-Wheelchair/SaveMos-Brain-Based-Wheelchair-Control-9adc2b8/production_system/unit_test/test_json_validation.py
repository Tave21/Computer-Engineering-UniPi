"""
Author: Alessandro Ascani
"""

import unittest
import os
import json
from unittest.mock import patch, mock_open, MagicMock
from production_system.json_validation import JsonHandler

class TestJsonHandler(unittest.TestCase):

    def setUp(self):
        """
        Set up resources for tests.
        """
        self.handler = JsonHandler()
        self.test_data = {"name": "Mario", "age": 30, "hobby": ["sport", "cooking"]}
        self.test_filepath = "test.json"
        self.test_schema_path = "schema.json"
        self.test_schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"},
                "hobby": {"type": "array", "items": {"type": "string"}}
            },
            "required": ["name", "age", "hobby"]
        }

    def tearDown(self):
        """
        Clean up any created files.
        """
        if os.path.exists(self.test_filepath):
            os.remove(self.test_filepath)
        if os.path.exists(self.test_schema_path):
            os.remove(self.test_schema_path)

    def test_write_json_file(self):
        """
        Test writing JSON data to a file.
        """
        result = self.handler.write_json_file(self.test_data, self.test_filepath)
        self.assertTrue(result)
        self.assertTrue(os.path.exists(self.test_filepath))

        with open(self.test_filepath, "r") as f:
            content = json.load(f)
        self.assertEqual(content, self.test_data)

    def test_read_json_file(self):
        """
        Test reading JSON data from a file.
        """
        with open(self.test_filepath, "w") as f:
            json.dump(self.test_data, f)

        content = self.handler.read_json_file(self.test_filepath)
        self.assertEqual(content, self.test_data)

    @patch("builtins.open", new_callable=mock_open, read_data='{"name": "Mario", "age": 30, "hobby": ["sport", "cooking"]}')
    def test_read_json_file_mock(self, mock_file):
        """
        Test reading JSON data using a mocked file.
        """
        content = self.handler.read_json_file(self.test_filepath)
        self.assertEqual(content, self.test_data)
        mock_file.assert_called_once_with(self.test_filepath, "r")

    def test_validate_json(self):
        """
        Test validating JSON data against a schema.
        """
        with open(self.test_schema_path, "w") as f:
            json.dump(self.test_schema, f)

        is_valid = self.handler.validate_json(self.test_data, self.test_schema_path)
        self.assertTrue(is_valid)

    def test_validate_json_invalid(self):
        """
        Test validation with invalid JSON data.
        """
        invalid_data = {"name": "Mario", "age": "thirty", "hobby": ["sport", 123]}
        with open(self.test_schema_path, "w") as f:
            json.dump(self.test_schema, f)

        is_valid = self.handler.validate_json(invalid_data, self.test_schema_path)
        self.assertFalse(is_valid)

if __name__ == "__main__":
    unittest.main()
