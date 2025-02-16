import unittest
import os
import json
from typing import Any

from utility.json_handler.json_handler import JsonHandler


class TestJsonHandler(unittest.TestCase):
    """
    A test class for testing the JsonHandler class.
    """

    def setUp(self):
        """
        Setup for each test case. Creates an instance of JsonHandler and
        prepares necessary test files.
        """
        self.handler = JsonHandler()
        self.test_file = "test.json"
        self.test_schema_file = "test_schema.json"
        self.sample_data = {"name": "Mario", "age": 30, "hobby": ["sport", "cooking"]}
        self.invalid_schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"}
            },
            "required": ["name", "age"]
        }

        # Prepare a valid schema for validation
        with open(self.test_schema_file, "w", encoding="utf-8") as schema_file:
            json.dump(self.invalid_schema, schema_file, ensure_ascii=False, indent=4)

    def tearDown(self):
        """
        Clean up after each test case. Remove the test file if it exists.
        """
        if os.path.exists(self.test_file):
            os.remove(self.test_file)

        if os.path.exists(self.test_schema_file):
            os.remove(self.test_schema_file)

        # Ensure cleanup for any files created during the tests
        if os.path.exists("valid_data.json"):
            os.remove("valid_data.json")
        if os.path.exists("invalid_data.json"):
            os.remove("invalid_data.json")

    def test_write_json_file(self):
        """
        Test the write_json_file function to ensure data is written correctly to a file.
        """
        result = self.handler.write_json_file(self.sample_data, self.test_file)
        self.assertTrue(result)

        # Verify file content
        with open(self.test_file, "r") as f:
            content = json.load(f)
        self.assertEqual(content, self.sample_data)

    def test_read_json_file(self):
        """
        Test the read_json_file function to ensure it reads data correctly from a file.
        """
        self.handler.write_json_file(self.sample_data, self.test_file)
        content = self.handler.read_json_file(self.test_file)
        self.assertEqual(content, self.sample_data)

    def test_write_field_to_json(self):
        """
        Test the write_field_to_json function to ensure it correctly updates a field in the JSON file.
        """
        self.handler.write_json_file(self.sample_data, self.test_file)
        updated_value = "traveling"
        field_name = "hobby"

        # Update the field
        result = self.handler.write_field_to_json(self.test_file, field_name, updated_value)
        self.assertTrue(result)

        # Verify the field was updated
        content = self.handler.read_json_file(self.test_file)
        self.assertEqual(content[field_name], updated_value)

    def test_read_field_from_json(self):
        """
        Test the read_field_from_json function to ensure it correctly reads a specific field.
        """
        self.handler.write_json_file(self.sample_data, self.test_file)
        field_name = "name"

        value = self.handler.read_field_from_json(self.test_file, field_name)
        self.assertEqual(value, self.sample_data[field_name])

    def test_get_system_address(self):
        """
        Test the get_system_address function to retrieve system configuration from a JSON file.
        """
        system_data = {
            "system1": {"ip": "192.168.1.10", "port": 8080},
            "system2": {"ip": "192.168.1.20", "port": 8081}
        }

        # Create a sample JSON with system data
        self.handler.write_json_file(system_data, self.test_file)

        # Test fetching data for an existing system
        result = self.handler.get_system_address(self.test_file, "system1")
        self.assertEqual(result, system_data["system1"])

        # Test fetching data for a non-existent system
        result = self.handler.get_system_address(self.test_file, "system3")
        self.assertIsNone(result)

    def test_validate_json(self):
        """
        Test the validate_json function to ensure it correctly validates a JSON object against a schema.
        """
        valid_data = {"name": "Mario", "age": 30}
        invalid_data = {"name": "Mario"}

        # Validate valid data
        result = self.handler.validate_json(valid_data, self.test_schema_file)
        self.assertTrue(result)

        # Validate invalid data
        result = self.handler.validate_json(invalid_data, self.test_schema_file)
        self.assertFalse(result)

    def test_validate_json_from_path(self):
        """
        Test the validate_json_from_path function to ensure it correctly validates a JSON file against a schema.
        """
        valid_data = {"name": "Mario", "age": 30}
        invalid_data = {"name": "Mario"}

        # Write valid and invalid data to files
        self.handler.write_json_file(valid_data, "valid_data.json")
        self.handler.write_json_file(invalid_data, "invalid_data.json")

        # Validate valid data
        result = self.handler.validate_json_from_path("valid_data.json", self.test_schema_file)
        self.assertTrue(result)

        # Validate invalid data
        result = self.handler.validate_json_from_path("invalid_data.json", self.test_schema_file)
        self.assertFalse(result)

        # Cleanup the test files created
        if os.path.exists("valid_data.json"):
            os.remove("valid_data.json")
        if os.path.exists("invalid_data.json"):
            os.remove("invalid_data.json")

    def test_dict_to_string(self):
        dict_str = self.handler.dict_to_string(self.sample_data)

        dict_from_str = self.handler.string_to_dict(dict_str)

        self.assertEqual(dict_from_str, self.sample_data)

    def test_string_to_dict(self):
        dict_from_str = self.handler.string_to_dict('''{
        "name": "Mario",
        "age": 30,
        "hobby": [
            "sport",
            "cooking"
        ]
    }''')
        self.assertEqual(dict_from_str, self.sample_data)

if __name__ == "__main__":
    unittest.main()
