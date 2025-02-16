"""
Author: Giovanni Ligato
"""


import json
import jsonschema


class EvaluationSystemParameters:
    """
    This class is used to store the parameters of the Evaluation System.
    """

    # Local parameters
    LOCAL_PARAMETERS_PATH = "parameters/evaluation_system_parameters.json"
    LOCAL_PARAMETERS_SCHEMA_PATH = "schemas/evaluation_system_parameters_schema.json"
    LOCAL_PARAMETERS = {}

    # Global parameters
    GLOBAL_PARAMETERS_PATH = "parameters/global_netconf.json"
    GLOBAL_PARAMETERS_SCHEMA_PATH = "schemas/global_netconf_schema.json"
    GLOBAL_PARAMETERS = {}

    @staticmethod
    def loadParameters(basedir: str = "."):
        """
        This method is used to load the parameters of the Evaluation System.

        :param basedir: The base directory from which to look for the different parameters files.
        """

        try:
            with open(f"{basedir}/{EvaluationSystemParameters.LOCAL_PARAMETERS_PATH}", "r") as local_params:
                EvaluationSystemParameters.LOCAL_PARAMETERS = json.load(local_params)

                if not EvaluationSystemParameters._validate_json(EvaluationSystemParameters.LOCAL_PARAMETERS, "local", basedir):
                    print("Invalid local parameters.")

            with open(f"{basedir}/{EvaluationSystemParameters.GLOBAL_PARAMETERS_PATH}", "r") as global_params:
                EvaluationSystemParameters.GLOBAL_PARAMETERS = json.load(global_params)

                if not EvaluationSystemParameters._validate_json(EvaluationSystemParameters.GLOBAL_PARAMETERS, "global", basedir):
                    print("Invalid global parameters.")

        except Exception as e:
            print(f"Error loading parameters: {e}")

    @staticmethod
    def _validate_json(json_parameters: dict, param_type: str, basedir: str = ".") -> bool:
        """
        Validate JSON parameters read from a file.

        :param json_parameters: The JSON parameters to validate.
        :param param_type: The type of the parameters (local or global).
        :param basedir: The base directory from which to look for the different parameters files.
        :return: True if the JSON parameters are valid, False otherwise.
        """

        if param_type == "local":
            schema_path = f"{basedir}/{EvaluationSystemParameters.LOCAL_PARAMETERS_SCHEMA_PATH}"
        elif param_type == "global":
            schema_path = f"{basedir}/{EvaluationSystemParameters.GLOBAL_PARAMETERS_SCHEMA_PATH}"
        else:
            return False

        with open(schema_path, "r") as schema_file:
            schema = json.load(schema_file)

        try:
            jsonschema.validate(json_parameters, schema)
            return True
        except jsonschema.ValidationError as e:
            if param_type == "local":
                print(f"Invalid local parameters: {e}")
            elif param_type == "global":
                print(f"Invalid global parameters: {e}")
            return False
