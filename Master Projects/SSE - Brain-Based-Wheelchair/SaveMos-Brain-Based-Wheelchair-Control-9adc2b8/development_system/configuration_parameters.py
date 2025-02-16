"""
Module: configuration_parameters
Represents the configuration parameters of the development system.

Author: Gabriele Pianigiani

"""
from development_system.json_validator_reader_and_writer import JsonValidatorReaderAndWriter


class ConfigurationParameters:
    """Class representing configuration parameters of the development system."""
    params = {}

    @staticmethod
    def load_configuration():
        """Load configuration parameters from a JSON file."""
        read_conf = JsonValidatorReaderAndWriter()  # instance of JsonHandler class
        read_conf.validate_json("conf/development_parameters.json", "schemas/development_parameters_schema.json")
        filepath = "conf/development_parameters.json"

        ConfigurationParameters.params = read_conf.read_configuration_parameters(filepath)
