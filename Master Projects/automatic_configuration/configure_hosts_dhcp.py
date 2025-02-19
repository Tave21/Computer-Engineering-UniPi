import sys
import json
import subprocess


def apply_configuration(config_dict):
    """
    Applies the configuration on the Alpine hosts.
    If the eth1 interface already has an IP address, it will be removed before requesting a new one.
    Additionally, it runs a series of network commands to properly configure the device.

    :param config_dict: Dictionary with host names and IP addresses.
    :return: List of results for each host.exi
    """
    results = []

    for hostname, ip_address in config_dict.items():
        dhcp_output = ""
        install_output = ""

        try:

            if hostname == "clab-network_topology-internet_host":
                ip_address = "128.128.128.128"
                interface = "eth1"

                # Verifica se l'indirizzo IP è presente prima di rimuoverlo
                try:
                    check_ip_command = f"docker exec {hostname} ip addr show dev {interface}"
                    ip_list = subprocess.check_output(check_ip_command, shell=True, stderr=subprocess.STDOUT, text=True)

                    if f"{ip_address}/24" in ip_list:
                        # Rimuove l'IP se esiste
                        remove_ip_command = f"docker exec {hostname} ip addr del {ip_address}/24 dev {interface}"
                        subprocess.check_call(remove_ip_command, shell=True, stderr=subprocess.STDOUT)
                        #print(f"Indirizzo IP {ip_address}/24 rimosso da {interface}")

                    # Aggiungi l'IP in ogni caso
                    add_ip_command = f"docker exec {hostname} ip addr add {ip_address}/24 dev {interface}"
                    subprocess.check_call(add_ip_command, shell=True, stderr=subprocess.STDOUT)
                    #print(f"Indirizzo IP {ip_address}/24 aggiunto a {interface}")

                except subprocess.CalledProcessError as e:
                   pass

            else:
                # Requests a new address via DHCP
                dhcp_command = f"docker exec {hostname} udhcpc -i eth1"
                #print(f"Executing DHCP on {hostname}: {dhcp_command}")
                dhcp_output = subprocess.check_output(dhcp_command, shell=True, stderr=subprocess.STDOUT, text=True)

                # MODIFICA - Debugging e parsing dell'indirizzo IP
                #print(f"DHCP output for {hostname}:\n{dhcp_output}")

                # Analizza l'output del comando DHCP per ottenere l'indirizzo IP
                for line in dhcp_output.splitlines():
                    if "lease of" in line and "obtained from" in line:
                        ip_address = line.split("lease of ")[1].split(" obtained")[0].strip()
                        print(f"New IP address for {hostname}: {ip_address}")
                        break

            try:
                install_command = f"docker exec {hostname} apk add iperf tcpdump"
                install_output = subprocess.check_output(install_command, shell=True, stderr=subprocess.STDOUT,
                                                         text=True)
            except subprocess.CalledProcessError:
                print("install nada...")

            # Retrieve routing information
            # Extract the gateway from the network configuration
            route_show_command = f"docker exec {hostname} ip route show"
            route_output = subprocess.check_output(route_show_command, shell=True, stderr=subprocess.STDOUT, text=True)
            #print(f"Routes found on {hostname}: {route_output}")

            # Extract the gateway and addresses from the default route
            default_route = [line for line in route_output.splitlines() if 'default' in line]

            if default_route:
                default_gateway = default_route[0].split()[2]
                remove_route_command = f"docker exec {hostname} ip route del default via {default_gateway}"
                #print(remove_route_command)
                subprocess.check_call(remove_route_command, shell=True, stderr=subprocess.STDOUT)

            # Adds the new default route
            if hostname == "clab-network_topology-internet_host":
                set_gateway_command = f"docker exec {hostname} ip route add default via 128.128.128.1"
                subprocess.check_call(set_gateway_command, shell=True, stderr=subprocess.STDOUT)
            else:
                # Retrieve the IP address of the network
                ip_parts = ip_address.split('.')
                x = ip_parts[2]  # ip.x for configuring the route
                # y = ip_parts[3]  # ip.y for configuring the route
                add_route_command = f"docker exec {hostname} ip route add default via 192.168.{x}.1"
                subprocess.check_call(add_route_command, shell=True, stderr=subprocess.STDOUT)

            # Removes the existing network route
            #remove_network_route_command = f"docker exec {hostname} ip route del 172.20.20.0/24 dev eth0 scope link src 172.20.20.{y}"
            #subprocess.check_call(remove_network_route_command, shell=True, stderr=subprocess.STDOUT)

            #print(f"Routing commands executed on {hostname}")

            results.append(f"Host {hostname} ({ip_address}): Success\n{dhcp_output}\n{install_output}")

        except subprocess.CalledProcessError as e:
            results.append(f"Host {hostname} ({ip_address}): Error\n{e.output}")
        except Exception as e:
            results.append(f"Host {hostname} ({ip_address}): Unknown error\n{str(e)}")

    return results


if __name__ == "__main__":
    # Check if the number of arguments is correct.
    if len(sys.argv) != 2:
        #print("Error: A JSON dictionary must be passed as an argument.")
        sys.exit(1)

    try:
        # Reads and deserializes the JSON dictionary passed as an argument.
        config_dict = json.loads(sys.argv[1])

        # Calls the function to apply the configuration.
        results = apply_configuration(config_dict)

        # Prints the outcome of the operation.
        for result in results:
            print(result)

    except json.JSONDecodeError as e:
        #print(f"Error in deserializing the JSON dictionary: {e}")
        sys.exit(1)
    except Exception as e:
        #print(f"Unknown error: {e}")
        sys.exit(1)
