import json
import subprocess
import sys
import threading


def send_command(config_dict):
    """
    Executes iperf client, server, and tcpdump commands on selected hosts based on the provided configuration.

    Parameters:
        config_dict (dict): A dictionary containing configuration details for the iperf client, server,
                             and tcpdump execution. The dictionary must contain:
                             - [0]['host']: The hostname of the client.
                             - [0]['ip']: The IP address of the client.
                             - [1]['host']: The hostname of the server.
                             - [1]['ip']: The IP address of the server.
                             - [2]: The name of the tcpdump device.
                             - [3]: The network interface for tcpdump on the tcpdump device.
                             - [4]: The transport protocol to use ("tcp" or "udp").

    Returns:
        dict: A dictionary with the results for each command (client, server, tcpdump).
              - 'client': Results of the iperf client command.
              - 'server': Results of the iperf server command.
              - 'tcpdump': Results of the tcpdump command.
    """
    command_results = {
        "client": "",
        "server": "",
        "tcpdump": ""
    }

    # Extract configuration values
    client_hostname = config_dict[0]['host']
    client_ip = config_dict[0]['ip']

    server_hostname = config_dict[1]['host']
    server_ip = config_dict[1]['ip']

    tcpdump_device_name = "clab-network_topology-" + config_dict[2]
    tcpdump_device_interface = config_dict[3]

    # Set transport protocol options based on provided protocol (tcp/udp)
    if config_dict[4] == "tcp":
        transport_protocol_option = ""
        tcpdump_filter = config_dict[4]
    elif config_dict[4] == "udp":
        transport_protocol_option = "-u"
        tcpdump_filter = config_dict[4]
    else:
        transport_protocol_option = ""
        tcpdump_filter = "tcp"

    # Set other iperf and tcpdump parameters
    port = "62000"
    timeout = "12"
    tcpdump_maximum_number_of_packets = "20"
    tentatives = "2"

    def run_client():
        """
        Runs the iperf client command on the client host.
        Captures the result of the iperf test to the server.
        """
        try:
            iperf_client_command = f"docker exec {client_hostname} iperf -c {server_ip} -i {tentatives} {transport_protocol_option} -p {port} -w 256k"
            iperf_client_output = subprocess.check_output(iperf_client_command, shell=True, stderr=subprocess.STDOUT,
                                                          text=True)
            command_results["client"] = f"Host {client_hostname} ({client_ip}): Success\n{iperf_client_output}"
        except subprocess.CalledProcessError as e:
            command_results["client"] = f"Host {client_hostname} ({client_ip}): Error\n{e.output}"
        except Exception as e:
            command_results["client"] = f"Host {client_hostname} ({client_ip}): Unknown error\n{str(e)}"

    def run_server():
        """
        Runs the iperf server command on the server host.
        The server will listen for connections from the client for the specified timeout duration.
        """
        try:
            iperf_server_command = f"docker exec {server_hostname} iperf {transport_protocol_option} -s -p {port} -t {timeout}"
            # The iperf server will run for 12 seconds.
            subprocess.check_output(iperf_server_command, shell=True, stderr=subprocess.STDOUT, text=True)
            command_results["server"] = f"Host {server_hostname} ({server_ip}): Server finished after {timeout} seconds"
        except subprocess.CalledProcessError as e:
            command_results["server"] = f"Host {server_hostname} ({server_ip}): Error\n{e.output}"
        except Exception as e:
            command_results["server"] = f"Host {server_hostname} ({server_ip}): Unknown error\n{str(e)}"

    def run_tcpdump():
        """
        Runs the tcpdump command on the tcpdump device.
        Captures network packets based on the specified transport protocol (tcp/udp) and port.
        """
        try:
            tcpdump_command = f"docker exec {tcpdump_device_name} timeout {timeout}s tcpdump -i {tcpdump_device_interface} {tcpdump_filter} port {port} -w /dev/null -c {tcpdump_maximum_number_of_packets}"
            # '-w /dev/null' prevents saving output to a file.
            # '-c 100' captures only a limited number of packets, to avoid an infinite loop.
            tcpdump_output = subprocess.check_output(tcpdump_command, shell=True, stderr=subprocess.STDOUT, text=True)
            command_results[
                "tcpdump"] = f"Host {tcpdump_device_name} ({tcpdump_device_interface}): Success\n{tcpdump_output}"
        except subprocess.CalledProcessError as e:
            command_results["tcpdump"] = f"Host {tcpdump_device_name} ({tcpdump_device_interface}): Error\n{e.output}"
        except Exception as e:
            command_results[
                "tcpdump"] = f"Host {tcpdump_device_name} ({tcpdump_device_interface}): Unknown error\n{str(e)}"

    # Create and start threads to run client, server, and tcpdump commands concurrently
    server_thread = threading.Thread(target=run_server)
    client_thread = threading.Thread(target=run_client)
    tcpdump_thread = threading.Thread(target=run_tcpdump)

    server_thread.start()
    client_thread.start()
    tcpdump_thread.start()

    # Wait for all threads to finish
    client_thread.join()
    server_thread.join()
    tcpdump_thread.join()

    return command_results


if __name__ == "__main__":
    """
    Main execution block that accepts a JSON string with configuration details.
    Calls the send_command function with the configuration and prints the results of each command.
    """
    # Check if the correct number of arguments is provided (must be 1 JSON argument)
    if len(sys.argv) != 2:
        sys.exit(1)

    try:
        # Deserialize the JSON argument into a Python dictionary
        config_dict = json.loads(sys.argv[1])

        # Call the send_command function with the provided configuration
        results = send_command(config_dict)

        # Print the results for each command (client, server, tcpdump)
        for command, result in results.items():
            print(f"--- {command.upper()} RESULT ---\n{result}\n")

    except json.JSONDecodeError:
        sys.exit(1)
    except Exception as e:
        sys.exit(1)
