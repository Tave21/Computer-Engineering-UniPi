
import sys
from production_system.json_validation import JsonHandler


class ConfigurationParameters:
    """
    Class that manage the configuration .
    """
    def __init__(self):

        handler = JsonHandler()
        path = "configuration/prod_sys_conf.json"
        prod_sys_conf = handler.read_json_file(path)
        schema_path = "production_schema/configSchema.json"
        if handler.validate_json(prod_sys_conf, schema_path) is False:
            print("json non valido")
            sys.exit(0)

        self.parameters = prod_sys_conf

        global_path = "configuration/global_netconf.json"
        netconf = handler.read_json_file(global_path)
        self.global_netconf = netconf




    def start_config(self):
        """

        Returns: dictionary with a field "configuration" that assume a string: "production"

        """

        configuration = {
            "configuration": "production"
        }

        return configuration