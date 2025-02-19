import json
import sys
from jsonrpclib import Server

USERNAME = "admin"
PASSWORD = "admin"

def execute_command_on_router(name, ip):
    """
    Connect to an Arista router via eAPI and execute a command to remove the default route.

    :param ip: IP address of the router.
    """

    server = Server(f"http://{USERNAME}:{PASSWORD}@{ip}:80/command-api")

    try:
        server.runCmds(1, ["enable"] + ["configure"] + ["no ip route 0.0.0.0/0 172.20.20.1"] + ["exit"] + [ "write"])
        #if the router is R3 add a default route in R3 that points R2
        if name[:2] == "R3":
            server.runCmds(1, ["enable"] + ["configure"] + ["ip route 0.0.0.0/0 172.20.50.0"] + ["exit"] + ["write"])
        elif name[:2] == "R4":
            server.runCmds(1, ["enable"] + ["configure"] + ["ip route 0.0.0.0/0 172.20.100.0"] + ["exit"] + ["write"])
        elif name[:2] == "internet_router":
            server.runCmds(1, ["enable"] + ["configure"] + ["ip route 0.0.0.0/0 172.20.100.1"] + ["exit"] + ["write"])


    except Exception as e:
        print(f"Error connecting to {ip}: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Error: You must pass a JSON dictionary as an argument.")
        sys.exit(1)

    # Deserialize the dictionary from the JSON string provided as a command-line argument
    router_map_file = json.loads(sys.argv[1])

    # Execute the command on each router in the map
    for name, ip in router_map_file.items():
        print(f"Executing command on {name} ({ip})")
        execute_command_on_router(name, ip)
