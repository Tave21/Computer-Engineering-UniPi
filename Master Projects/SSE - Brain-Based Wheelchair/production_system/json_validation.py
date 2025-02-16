import json
import logging
from typing import Any

import jsonschema

class JsonHandler:
    """
        A class to read and save file json
    """


    def read_json_file(self, filepath):
        """
        Read a json file.
        Args:
                filepath: path where is placed file json to read.

        Returns the content of the json file.

        """

        try:
            with open(filepath, "r") as f:
                return json.load(f)

        except Exception as e:
            print("Error to read file at path " + filepath + ": " + e)
            return None

    def write_json_file(self, data, filepath):
        """
            Args:
                data: data to write into json file
                filepath: path where json file will be saved.

            Returns:
                bool: True if the file is written successfully, False otherwise.
        """


        try:
            with open(filepath, "w") as f:
                json.dump(data, f, ensure_ascii=False, indent=4)
                return True
        except Exception as e:
            print("Error to save file at path " + filepath + ": " + e)
            return False

    def validate_json(self ,json_data: dict, schema_path: str) -> bool:
        """
            Validate a json object against a json schema in a file.
            Args:
                json_data: json object
                schema_path: path to the json schema relative to the data folder
            Return:
                bool: True if json object is valid, False otherwise
            """
        with open(schema_path, "r", encoding="UTF-8") as file:
            json_schema = json.load(file)
        try:
            jsonschema.validate(instance=json_data, schema=json_schema)
        except jsonschema.exceptions.ValidationError as ex:
            logging.error(ex)
            return False
        return True



# Example to test the class
if __name__ == "__main__":
    handler = JsonHandler()

    # Writing a json file
    data = {"name": "Mario", "age": 30, "hobby": ["sport", "coocking"]}
    handler.write_json_file(data, "esempio.json")

    # Reading a json file
    try:
        content = handler.read_json_file("esempio.json")
        print(content)
    except Exception as e:
        print(f"Errore: {e}")
