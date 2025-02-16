"""
Module: PreparationSystemParameters
Loads and manages configuration parameters for the preparation system.

Author: Francesco Taverna

"""
import sys

from preparation_system.preparation_json_handler.json_handler import JsonHandler
from preparation_system import ING_MAN_CONFIG_FILE_PATH, PREP_MAN_CONFIG_SCHEMA_FILE_PATH


class PreparationSystemParameters:
    """
    Loads and stores configuration parameters for the preparation system.
    """

    def __init__(self):
        """
        Load from a configuration file.
        """
        handler = JsonHandler()
        # read configuration file
        self.configuration = handler.read_json_file(ING_MAN_CONFIG_FILE_PATH)
        # validate configuration file schema
        is_valid = handler.validate_json(self.configuration, PREP_MAN_CONFIG_SCHEMA_FILE_PATH)
        if is_valid is False:
            sys.exit(0)