import sys
import json
import os
from jsonrpclib import Server

# Credentials for connecting to the Arista router
USERNAME = "admin"
PASSWORD = "admin"

# Function to read the configuration file
def read_config_file(file_path):
    """
    Reads the content of a configuration file.

    Args:
        file_path (str): The path to the configuration file.

    Returns:
        str: The content of the file if successfully read, or an error message if an issue occurs.
    """
    try:
        # Check if the file exists and then read it
        if os.path.isfile(file_path):
            with open(file_path, 'r') as file:
                return file.read()
        else:
            return f"Error: File {file_path} not found."
    except Exception as e:
        return f"Error reading the file {file_path}: {e}"


# Function to apply the configuration to the router
def apply_configuration(config_dict):
    """
    Applies configuration commands to the specified routers.

    Args:
        config_dict (dict): A dictionary mapping configuration file names to router IP addresses.

    Returns:
        list: A list of results indicating success or failure for each router.
    """
    results = []

    # Get the absolute path of the current script
    script_dir = os.path.dirname(os.path.realpath(__file__))

    for config_file, router_ip in config_dict.items():
        # Construct the full path to the configuration file
        config_file_path = os.path.join(script_dir, config_file)

        # Connect to the JSON-RPC server of the router
        server = Server(f"http://{USERNAME}:{PASSWORD}@{router_ip}:80/command-api")

        try:
            # Read the content of the configuration file
            config_content = read_config_file(config_file_path)
            if config_content.startswith("Error"):
                results.append(f"Error with file {config_file}: {config_content}")
                continue

            # Split the configuration into individual commands
            commands = config_content.splitlines()

            # Send the commands to the router
            server.runCmds(1, ["enable"] + ["configure"] + commands)

            # Save the current configuration to the startup configuration (optional)
            server.runCmds(1, ["enable"] + ["write"])

            results.append(f"Configuration successfully applied to {router_ip}.")

        except Exception as e:
            results.append(f"Error applying configuration to {router_ip}: {e}")

    return results


# Main execution block
if __name__ == "__main__":
    """
    The script expects a JSON dictionary as a command-line argument.
    The dictionary should map configuration file names to router IP addresses.

    Example usage:
        python script.py '{"config1.txt": "192.168.1.1", "config2.txt": "192.168.1.2"}'
    """
    if len(sys.argv) != 2:
        print("Error: You must pass a JSON dictionary as an argument.")
        sys.exit(1)

    try:
        # Deserialize the dictionary from the JSON string provided as a command-line argument
        config_dict = json.loads(sys.argv[1])
        results = apply_configuration(config_dict)

        # Print the results of the operation
        for result in results:
            print(result)

    except json.JSONDecodeError as e:
        print(f"Error parsing the JSON dictionary: {e}")
        sys.exit(1)
