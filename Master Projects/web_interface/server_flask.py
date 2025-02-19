import json
import os
import re

import paramiko
import yaml
from flask import Flask, request, jsonify
from flask_cors import CORS

from template.create_configuration_file import create_file
from yaml_to_json_converter import process_topology

# Global variables
VM_IP = "10.1.1.162"
USERNAME = "root"
PASSWORD = "root"
REMOTE_DIR = "project"
CLAB_NAME = "clab-network_topology"

TOPOLOGY_YAML_FILE_PATH = "../template/network_configuration_and_topology/topology_task2.yaml"
NETWORK_CONFIGURATION_YAML_FILE_PATH = "../template/network_configuration_and_topology/network_configuration.yaml"

# Initialize Flask application
app = Flask(__name__)

# Enable Cross-Origin Resource Sharing (CORS) to allow requests from different domains
CORS(app)

# Define the directory where files will be uploaded and saved
UPLOAD_DIR = os.path.abspath('../template')
USER_CONTENT_UPLOAD_DIR = os.path.abspath('../template/network_configuration_and_topology')
REMOTE_SCRIPTS_DIR = os.path.abspath('../automatic_configuration')
UPLOAD_DIR_CONFIGURATIONS = os.path.join(UPLOAD_DIR, "configurations")
UPLOAD_DIR_BUSINESS_RULES = os.path.join(UPLOAD_DIR, "business_rules")
os.makedirs(UPLOAD_DIR, exist_ok=True)
os.makedirs(UPLOAD_DIR_BUSINESS_RULES, exist_ok=True)
os.makedirs(UPLOAD_DIR_CONFIGURATIONS, exist_ok=True)


@app.route('/upload', methods=['POST'])
def upload_file():
    """
    This route handles the file upload, saves the uploaded file to a directory,
    and then deploys containerlab using the uploaded file.

    Returns:
        JSON response indicating the result of the deployment process.
    """
    #print("Request received.")

    # Check if the file is present in the request
    if 'networkFile' not in request.files:
        #print("No file part in request")
        return jsonify({"error": "No file part"}), 400

    # Get the file from the request
    file = request.files['networkFile']

    # Check if the file has a filename
    if file.filename == '':
        #print("No selected file")
        return jsonify({"error": "No selected file"}), 400

    #print("file name: ", file.filename)

    # Save the file to the specified upload directory
    file_path = os.path.join(UPLOAD_DIR, file.filename)
    file.save(file_path)

    #print(f"File saved to {file_path}. Starting deploy...")

    # Call deploy_containerlab function to deploy the containerlab configuration
    result = deploy_containerlab(file_path)

    file_names = get_file_names("../automatic_configuration")
    send_files(file_names , "three")

    # Return the deployment result as a JSON response
    return jsonify({"result": result})


def deploy_containerlab(local_conf_path):
    """
    Deploys a ContainerLab network topology to a remote VM.

    Args:
        local_conf_path (str): Path to the local configuration file.

    Returns:
        str: Result message indicating success or failure of the deployment.
    """
    try:
        # Create an SSH client to connect to the remote machine
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh.connect(VM_IP, username=USERNAME, password=PASSWORD)

        # Create the remote directory if it doesn't exist
        #print("remote dir: ", REMOTE_DIR)
        #print("local_conf_path: ", local_conf_path)
        ssh.exec_command(f'mkdir -p {REMOTE_DIR}')

        # Open SFTP connection for file transfer
        sftp = ssh.open_sftp()

        # Correct directory separator for remote path
        remote_dir = REMOTE_DIR.replace("\\", "/")

        # Ensure the remote directory ends with a slash
        if not remote_dir.endswith("/"):
            remote_dir += "/"

        # Build the full path for the remote configuration file
        remote_conf_path = os.path.join(remote_dir, 'conf.clab.yaml')

        # Transfer the local configuration file to the remote machine
        sftp.put(local_conf_path, remote_conf_path)

        # Close the SFTP connection
        sftp.close()

        # Execute the command to deploy the containerlab configuration on the remote VM
        stdin, stdout, stderr = ssh.exec_command('cd project && sudo containerlab deploy --reconfigure')
        #print("Executing deployment")
        exit_status = stdout.channel.recv_exit_status()
        #print(f"Error: {stderr.read().decode()}")

        ssh.close()

        # Check if the deployment was successful
        if exit_status == 0:
            #print("Deploy completed successfully")
            #Create the json file that is used to view the network topology
            process_topology()
            return "Deploy completed successfully."
        else:
            return f"Deploy failed with exit status {exit_status}. Error: {stderr.read().decode()}"

    except Exception as e:
        return f"A Flask error occurred: {str(e)}"

def get_file_names(directory):
    """
        Returns a list of all file names in the specified directory.
        Parameters:
        directory (str): The path to the directory from which file names will be retrieved.
        Returns:
        list: A list containing the names of all files in the given directory.
    """
    file_names = [f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))]
    return file_names

@app.route('/send-config', methods=['POST'])
def upload_configuration_file():
    """
    This route handles the uploading of configuration files and initiates the creation
    of configurations or splitting of router configurations.

    Returns:
        JSON response indicating the success or failure of the operation.
    """
    #print("Request received.")

    # Check if the file is present in the request
    if 'networkFile' not in request.files:
        #print("No file part in request")
        return jsonify({"error": "No file part"}), 400

    # Get the file from the request
    file = request.files['networkFile']
    file_type = request.form.get('type')

    # Check if the file has a filename
    if file.filename == '':
        #print("No selected file")
        return jsonify({"error": "No selected file"}), 400

    #print("file name: ", file.filename)

    try:
        file_path = os.path.join(USER_CONTENT_UPLOAD_DIR, file.filename)
        print("file type = ", file_type)
        print("file path = ", file_path)

        # Call create_file or split_router_configs based on the type of request
        if file_type == "one":
            generated_files_conf = create_file(file_path)
        elif file_type == "two":
            generated_files_conf = split_router_configs(file_path, UPLOAD_DIR_BUSINESS_RULES)

        #print(f"Generated files: {generated_files_conf}")

        # Send the generated files to the remote machine via SFTP.
        send_files(generated_files_conf , file_type)

        # Get the IP addresses of the containers associated with the files.
        file_to_ip_map = get_container_ips(generated_files_conf)

        # Execute the remote script to apply the configuration
        remote_script = REMOTE_DIR + "/send_configuration.py"
        result = execute_remote_script(file_to_ip_map, remote_script)
        print("result =", result)
        if file_type == "one":
            # Activate DHCP on hosts if the file type is "one"
            activate_dhcp_on_hosts()
        elif file_type == "two":
            remote_script = REMOTE_DIR + "/delete_default_route.py"
            result = execute_remote_script(file_to_ip_map, remote_script)
        return jsonify({"success": True, "result": result})

    except Exception as e:
        #print(f"Error occurred: {e}")
        return jsonify({"success": False, "error": "An error occurred during file processing"}), 500




def send_files(files , file_type):
    """
    Sends files to a remote machine via SFTP.
        :param files:
        :param file_type:
    """
    try:
        # Create an SFTP connection to the remote machine
        transport = paramiko.Transport((VM_IP, 22))
        transport.connect(username=USERNAME, password=PASSWORD)

        # Correct directory separator for remote path
        remote_dir = REMOTE_DIR.replace("\\", "/")

        # Ensure the remote directory ends with a slash
        if not remote_dir.endswith("/"):
            remote_dir += "/"

        # Create an SFTP client
        sftp = paramiko.SFTPClient.from_transport(transport)

        for file_name in files:
            # Build the full path for the local and remote files.
            if file_type == "one":
                local_file_path = os.path.join(UPLOAD_DIR_CONFIGURATIONS, file_name)
            elif file_type == "two":
                local_file_path = os.path.join(UPLOAD_DIR_BUSINESS_RULES, file_name)
            elif file_type == "three":
                local_file_path = os.path.join(REMOTE_SCRIPTS_DIR, file_name)

            remote_file_path = os.path.join(remote_dir, file_name)
            print("local file ", local_file_path)
            # Send the file to the remote machine
            sftp.put(local_file_path, remote_file_path)
            #print(f"File {local_file_path} successfully sent to {remote_file_path}")

        # Close the SFTP connection
        sftp.close()
        transport.close()

        return "All files sent successfully"

    except Exception as e:
        #print(f"Error during file transfer via SFTP: {e}")
        return f"Error during sending files via SFTP: {e}"


@app.route('/ips', methods=['POST'])
def get_ips():
    """
    Retrieves IP addresses of containers on a remote VM.

    Returns:
        JSON response containing a mapping of file names to container IPs.
    """
    return jsonify({"success": True, "result": get_host_container_ips()})


def get_container_ips(file_names):
    """
    Retrieves the IP addresses of container interfaces (eth0) and associates them with file names.

    Args:
        file_names (list): List of file names (e.g., ["R1_configuration.txt", ...]).

    Returns:
        dict: Dictionary mapping file names to container IPs.
    """
    # Process file names to create container names
    container_suffixes = [
        f"{name.split('_')[0]}_{name.split('_')[1]}" if name.split('_')[0].lower() == "internet" else name.split('_')[0].lower()
        for name in file_names
    ]
    container_names = [f"{CLAB_NAME}-{suffix}" for suffix in container_suffixes]

    # Initialize dictionary to store file-to-IP mapping
    file_to_ip_map = {}

    try:
        # SSH connection to the remote machine
        ssh_client = paramiko.SSHClient()
        ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh_client.connect(hostname=VM_IP, username=USERNAME, password=PASSWORD)

        # Retrieve IP address for each container
        for file_name, container_name in zip(file_names, container_names):
            cmd = f"docker inspect -f '{{{{ .NetworkSettings.Networks.clab.IPAddress }}}}' {container_name}"
            stdin, stdout, stderr = ssh_client.exec_command(cmd)
            ip_address = stdout.read().decode().strip()  # Read the IP address from the output
            if ip_address:
                file_to_ip_map[file_name] = ip_address
            else:
                error = stderr.read().decode().strip()
                print(f"Error retrieving IP for container {container_name}: {error}")
                file_to_ip_map[file_name] = None  # No IP found, store None

        # Close SSH connection
        ssh_client.close()

    except Exception as e:
        print(f"Error retrieving container IPs: {e}")
        return {"error": str(e)}

    return file_to_ip_map


@app.route('/router_and_hosts_interfaces', methods=['POST'])
def extract_devices_and_interfaces(file_path=TOPOLOGY_YAML_FILE_PATH):
    """
    Estrae i nomi dei router, switch e le loro interfacce dal file YAML.

    :param file_path: Percorso al file topology_task2.yaml.
    :return: Dizionario con dispositivi e interfacce.
    """
    with open(file_path, 'r') as file:
        topology_data = yaml.safe_load(file)

    devices = {}

    # Itera tra i collegamenti definiti nei links
    links = topology_data['topology']['links']
    for link in links:
        for endpoint in link['endpoints']:
            device, interface = endpoint.split(":")
            if device not in devices:
                devices[device] = []
            if interface not in devices[device]:
                devices[device].append(interface)

        # Converte il dizionario in formato JSON
    devices_json = json.dumps(devices, indent=4)

    return devices_json


@app.route('/set_iperf_test', methods=['POST'])
def set_iperf_test():
    """
    Flask endpoint to configure an iPerf test between two hosts.
    Expects JSON data with keys: host1, host2, router, routerInterface, transportType.
    """
    try:
        # Parse the JSON request data
        data = request.get_json()

        # Extract required parameters
        host1 = data.get('host1')
        host2 = data.get('host2')
        router = data.get('router')
        routerInterface = data.get('routerInterface')
        transportType = data.get('transportType')

        # Validate that all parameters are provided
        if not all([host1, host2, router, routerInterface, transportType]):
            return jsonify({"success": False, "error": "Missing required fields"}), 400

        # Trigger the iPerf test (assumes the function is implemented elsewhere)
        result_op = activate_iperf_couple(host1, host2, router, routerInterface, transportType)

        return jsonify({"success": True, "result": result_op})

    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


def get_linux_hosts_from_yaml(file_path=TOPOLOGY_YAML_FILE_PATH):
    """
    Reads host names from the provided YAML file, filtering those with `kind: linux`.

    :param file_path: Path to the YAML file.
    :return: List of host names with `kind: linux`.
    """
    try:
        with open(file_path, 'r') as yaml_file:
            data = yaml.safe_load(yaml_file)  # Load YAML data into a Python dictionary

            # Verify that the YAML structure is valid
            if 'topology' in data and 'nodes' in data['topology']:
                nodes = data['topology']['nodes']

                # Filter nodes with `kind: linux`
                linux_hosts = [name for name, properties in nodes.items() if
                               properties.get('kind', '').lower() == 'linux']
                return linux_hosts
            else:
                raise ValueError("Invalid YAML structure: 'topology.nodes' not found.")
    except Exception as e:
        print(f"Error reading the YAML file: {e}")
        return []


def get_host_container_ips():
    """
    Retrieve the IP addresses of eth1 interfaces for containers and map them to their names.

    Returns:
        dict: A dictionary mapping container names to IP addresses or error messages.
    """
    # Define the container suffixes and names
    container_suffixes = get_linux_hosts_from_yaml()
    container_names = [f"{CLAB_NAME}-{suffix}" for suffix in container_suffixes]
    #print(container_names)
    file_to_ip_map = {}

    try:
        # Establish SSH connection
        ssh_client = paramiko.SSHClient()
        ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh_client.connect(hostname=VM_IP, username=USERNAME, password=PASSWORD)

        for host in container_names:
            try:
                # Command to get the eth1 IP address
                cmd = f"docker exec {host} ip -4 addr show eth1 | grep 'inet ' | awk '{{print $2}}' | cut -d/ -f1"
                stdin, stdout, stderr = ssh_client.exec_command(cmd)

                # Parse the output for the IP address
                ip_address = stdout.read().decode().strip()
                if ip_address:
                    file_to_ip_map[host] = ip_address
                else:
                    error = stderr.read().decode().strip()
                    file_to_ip_map[host] = f"Error retrieving IP: {error}" if error else "No IP found"

            except Exception as cmd_error:
                file_to_ip_map[host] = f"Command error: {cmd_error}"

        # Close the SSH connection
        ssh_client.close()

    except Exception as e:
        return f"SSH connection error: {e}"

    return file_to_ip_map


def activate_dhcp_on_hosts():
    """
    Enables the DHCP client on the eth1 interface of specified Linux containers.

    Returns:
        dict: A dictionary with results of the DHCP activation process for each container.
    """
    remote_script = REMOTE_DIR + "/configure_hosts_dhcp.py"

    try:
        # Get container IPs
        file_to_ip_map = get_host_container_ips()
        print(file_to_ip_map)
        if isinstance(file_to_ip_map, str) and file_to_ip_map.startswith("Error"):
            return {"success": False, "error": file_to_ip_map}

        # Execute the remote script
        result = execute_remote_script(file_to_ip_map, remote_script)
        print("result =", result)
        return result

    except Exception as e:
        return {"success": False, "error": f"Error activating DHCP: {e}"}


def activate_iperf_couple(host1, host2, tcpdump_device, tcpdump_interface, traffic):
    """
    Sets up an iperf test between two hosts and captures traffic using tcpdump.

    Args:
        host1 (str): The name of the first container.
        host2 (str): The name of the second container.
        tcpdump_device (str): The tcpdump device to capture traffic on.
        tcpdump_interface (str): The interface on the tcpdump device.
        traffic (str): The type of traffic to generate (e.g., UDP, TCP).

    Returns:
        dict: A dictionary with the results of the iperf test and tcpdump execution.
    """
    remote_script = REMOTE_DIR + "/iperf_and_tcpdump_command_sender.py"

    try:
        # Get container IPs
        ips = get_host_container_ips()
        if isinstance(ips, str) and ips.startswith("Error"):
            return {"success": False, "error": ips}

        # Validate IPs for both hosts
        host1_ip = ips.get(host1)
        host2_ip = ips.get(host2)

        if not host1_ip or not host2_ip:
            missing_hosts = [host for host, ip in [(host1, host1_ip), (host2, host2_ip)] if not ip]
            return {"success": False, "error": f"Missing IPs for hosts: {', '.join(missing_hosts)}"}

        # Build the configuration map for the remote script
        file_to_ip_map = [
            {"host": host1, "ip": host1_ip},
            {"host": host2, "ip": host2_ip},
            tcpdump_device,
            tcpdump_interface,
            traffic,
        ]

        # Execute the remote script
        return execute_remote_script(file_to_ip_map, remote_script)

    except Exception as e:
        return {"success": False, "error": f"Error activating iperf test: {e}"}


def execute_remote_script(config_dict, remote_script_path):
    """
    Executes a Python script located on a remote machine via SSH, passing a JSON-serialized dictionary as an argument.

    Args:
        config_dict (dict): The configuration dictionary to pass as an argument to the script.
        remote_script_path (str): The path to the Python script on the remote machine.
    Returns:
        str: The output of the remote script if the execution is successful.
        str: The error output of the remote script if any error occurs during execution.

    Raises:
        Exception: If there are issues with the SSH connection or command execution.
    """
    try:
        # Initialize the SSH client
        ssh = paramiko.SSHClient()

        # Automatically accept unknown SSH host keys
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

        # Connect to the remote machine using the provided credentials
        ssh.connect(VM_IP, username=USERNAME, password=PASSWORD)

        # Serialize the dictionary to JSON format
        json_dict = json.dumps(config_dict)

        # Construct the command to execute the script on the remote machine
        # The JSON dictionary is passed as a command-line argument
        command = f"python3 {remote_script_path} '{json_dict}'"

        print(command)

        # Execute the command remotely
        stdin, stdout, stderr = ssh.exec_command(command)

        # Read the standard output from the command
        output = stdout.read().decode()

        # Read the standard error (if any) from the command
        error_output = stderr.read().decode()

        # Close the SSH connection
        ssh.close()

        # Return the error output if an error occurred
        if error_output:
            return error_output

        # Return the standard output if the command executed successfully
        return output

    except Exception as e:
        # Handle and return errors related to SSH connection or execution
        return f"Error in the SSH connection o during the execution of the remote command: {e}"


def split_router_configs(input_file_path, output_directory):
    """
    Splits a configuration file containing rules for multiple routers into separate files for each router.

    Args:
        input_file_path (str): The path to the input file containing router configurations.
        output_directory (str): The directory where the split configuration files will be saved.

    Returns:
        list: A list of file names created in the output directory.
              Returns an empty list if an error occurs.

    Raises:
        Exception: If file operations or parsing encounters issues.
    """
    # List to store the names of the created files
    file_names = []

    try:
        # Ensure the output directory exists; create it if it doesn't
        os.makedirs(output_directory, exist_ok=True)

        # Read the entire content of the input configuration file
        with open(input_file_path, 'r') as file:
            content = file.read()

        # Split the content into sections for each router
        # Sections are identified by the pattern "//R<number>"
        router_sections = re.split(r'(//R\d+|//internet_router)', content)

        # Group sections into router name and corresponding content
        routers = []
        for i in range(1, len(router_sections), 2):
            router_name = router_sections[i].strip()  # e.g., "//R1"
            router_content = router_sections[i + 1]  # The configuration content
            routers.append((router_name, router_content))

        # Process each router's configuration
        for router_name, router_content in routers:
            # Remove comments from the configuration content (lines starting with //)
            clean_content = re.sub(r'//.*', '', router_content).strip()

            # Create the output file name based on the router identifier (e.g., "R1_business_rules.txt")
            file_name = f"{router_name[2:]}_business_rules.txt"
            output_file_path = os.path.join(output_directory, file_name)

            # Add the file name to the list
            file_names.append(file_name)

            # Write the cleaned content to the output file
            with open(output_file_path, 'w') as output_file:
                output_file.write(clean_content)

        print(f"Configurations have been split and saved in the directory: {output_directory}")
        return file_names

    except Exception as e:
        # Handle and log any exceptions that occur
        print(f"Error during configuration splitting: {e}")
        return []


@app.route('/destroy', methods=['POST'])
def destroy_containerlab():
    """
    Handles the destruction of the containerlab network deployment.
    """
    print("Request received.")

    try:
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh.connect(VM_IP, username=USERNAME, password=PASSWORD)

        stdin, stdout, stderr = ssh.exec_command(
            'cd project && sudo containerlab destroy && cd .. && rm -r project'
        )
        print("Destroy deployment initiated.")

        exit_status = stdout.channel.recv_exit_status()
        command_output = stdout.read().decode()
        error_output = stderr.read().decode()

        ssh.close()

        if exit_status == 0:
            print("Destruction completed successfully.")
            return {
                "status": "success",
                "result": "Network destruction completed successfully.",
                "output": command_output.strip(),
            }
        else:
            print(f"Destruction failed with exit status {exit_status}. Error: {error_output.strip()}")
            return {
                "status": "failure",
                "result": f"Network destruction failed. Error: {error_output.strip()}",
                "exit_status": exit_status,
                "output": error_output.strip(),
            }

    except Exception as e:
        print(f"An exception occurred: {e}")
        return {
            "status": "error",
            "result": f"An exception occurred: {str(e)}",
            "message": str(e),
        }


@app.errorhandler(500)
def internal_error(error):
    return jsonify({"error": "Internal server error"}), 500


@app.errorhandler(404)
def not_found_error(error):
    return jsonify({"error": "Not found"}), 404


if __name__ == '__main__':
    app.run(debug=True)
    #activate_dhcp_on_hosts()
    #print(get_host_container_ips())
