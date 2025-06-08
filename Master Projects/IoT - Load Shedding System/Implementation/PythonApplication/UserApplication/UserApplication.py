# Import necessary libraries
from configparser import ConfigParser
from Utility.DBAccess import DBAccess
from Utility.Log import Log
from tabulate import tabulate
from coapthon.client.helperclient import HelperClient
import os
import json

# Create a logger for the User module
log = Log(__name__, "User").get_logger()

# Define the Node class
class Node:
    def __init__(self, node_tuple, id):
        # Initialize node with id, IP, name, and settings
        self.id = id
        self.ip = node_tuple[0]
        self.name = node_tuple[1]
        self._decimal_accuracy = 10000
        # Load settings from JSON string
        self.settings = json.loads(node_tuple[2])
        self.modified = False
        self._keys = list(self.settings.keys())

    # Print all settings for this node
    def print_settings(self):
        print(f"\n[{self.id}-{self.name.upper()}]")
        i = 0
        for key, value in self.settings.items():
            print(f"\t[{i}] {key}: {value / self._decimal_accuracy}")
            i += 1
    
    # Change a specific setting by parameter index
    def change_setting(self, parameter_id, new_value):
        print(f"Changing setting {self._keys[parameter_id]} to {new_value} (not multiplied yet)")
        self.settings[self._keys[parameter_id]] = int(new_value * self._decimal_accuracy)
        self.modified = True

    # String representation of the node
    def __str__(self):
        return f"IP: {self.ip}, Name: {self.name}, Settings: {self.settings}"

# Define the main UserApplication class
class UserApplication:
    def __init__(self):
        # Read configuration file
        self.decimal_accuracy = 10000
        configur = ConfigParser()
        configur.read('./CoAPServer/config.ini')
        
        # Initialize database access with parameters from config file
        self.db = DBAccess(
            host = configur.get('mysql', 'host'),
            user = configur.get('mysql', 'user'),
            password = configur.get('mysql', 'password'),
            database = configur.get('mysql', 'database'),
            log=log)

    # Start the application main loop
    def start(self):
        while True:
            self.print_menu()
            command = input("Command: ")
            if command == "1":
                self.nodes_settings()
            elif command == "2":
                self.list_nodes()
            elif command == "3":
                self.invert()
            elif command == "4":
                self.check()
            elif command == "5":
                self.consult()
            elif command == "6":
                self.history()
            elif command == "7":
                break
            else:
                print("Invalid command")

    # Check the current status of the relay
    def check(self):
        self.clear()
        self.db.connect()
        rows= self.db.query(query="SELECT status FROM relay ORDER BY timestamp DESC, id LIMIT 1", val=None, fetchall=True)
        self.db.close()
        if rows and rows[0][0] == 1:
            print("Relay is ON")
        else:
            print("Relay is OFF")
        input("Press any key to continue...")

    # Consult historical status data of the relay
    def consult(self):
        self.clear()
        self.db.connect()
        values = self.db.query(query="SELECT timestamp, status FROM relay ORDER BY timestamp DESC, id LIMIT 12", val=None, fetchall=True)
        self.db.close()
        
        if not values:
            print("No historical data available.")
            input("Press any key to continue...")
            return
        
        # Print the historical data in a table format
        headers = ["Timestamp", "Status"]
        print(tabulate(values, headers=headers, tablefmt="psql"))
        input("Press any key to continue...")

    # Consult historical load data of the nodes
    def history(self):
        self.clear()
        self.db.connect()
        values = self.db.query(query="SELECT timestamp, sampled, predicted FROM `load` ORDER BY timestamp DESC LIMIT 12", val=None, fetchall=True)
        self.db.close()
        
        if not values:
            print("No historical load data available.")
            input("Press any key to continue...")
            return
        
        # Print the historical load data in a table format
        headers = ["Timestamp", "Sampled", "Predicted"]
        print(tabulate(values, headers=headers, tablefmt="psql"))
        input("Press any key to continue...")

    # Invert the relay status for the DR manager node
    def invert(self):
        self.clear()
        value = 0
        self.db.connect()
        nodes = self.db.query(query="SELECT ip, name, settings FROM nodes", val=None, fetchall=True)
        for ip, name, settings_json in nodes:
            if name == "DR-manager":
                # Invert the relay status for the DR manager node
                latest = self.db.query(query="SELECT status FROM relay ORDER BY id DESC LIMIT 1", val=None, fetchall=True)
                if not latest or latest[0][0] == 0:
                    value = 1
                else:
                    value = 0

                # The new status is saved in the database thanks to the notification mechanism

                # Send the new settings to the CoAP node
                client = HelperClient(server=(ip, 5683))
                client.put("status", json.dumps({"status": value}))
                client.stop()
                break
        self.db.close()
        print("Relay status inverted!")
        input("Press any key to continue...")

    # Print the main menu
    def print_menu(self):
        self.clear()
        print("||======================MENU=====================================||")
        print("||Enter the corresponding command number:                        ||")
        print("|| [1] Nodes settings                                            ||")
        print("|| [2] List nodes                                                ||")
        print("|| [3] Invert relay status                                       ||")
        print("|| [4] Check relay status                                        ||")
        print("|| [5] Consult historical status data                            ||")
        print("|| [6] Consult historical load data                              ||")
        print("|| [7] Exit                                                      ||")
        print("||===============================================================||")

    # Show node settings and accept changes
    def setting_menu(self, nodes):
        self.clear()
        print("||======================NODES SETTINGS============================||")
        for node in nodes:
            node.print_settings()           
        print("[s] save changes")
        print("[q] back")
        input_cmd = input("Insert the new value of the chosen parameter (format: [node_id] [parameter_id] [new_value]) or [q] to go back: ")
        return input_cmd

    # Save modified settings to database and CoAP nodes
    def save_settings(self, nodes):
        print("Saving settings...")
        # Update settings in the database
        for node in nodes:
            if node.modified:
                self.db.query(query="UPDATE nodes SET settings = %s WHERE ip = %s", val=(json.dumps(node.settings), node.ip))
        # Send updated settings to each modified node
        print("Sending the new settings: ", end="")
        for node in nodes:
            if node.modified:
                client = HelperClient(server=(node.ip, 5683))
                print(f"\n \t{node.name} - {node.ip}")
                print("MOFIFYING with:")
                print(node.settings)
                client.put("settings", json.dumps(node.settings))
                client.stop()
        print("Settings saved!")
        input("Press any key to continue...")

    # Manage settings for all nodes
    def nodes_settings(self):
        self.db.connect()
        sql_nodes = self.db.query(query="SELECT ip, name, settings FROM nodes", val=None, fetchall=True)
        nodes = []
        i = 0
        # Create Node objects from query result
        for node in sql_nodes:
            nodes.append(Node(node, i))
            i += 1
        
        input_cmd = ""
        while input_cmd != "q":
            input_cmd = self.setting_menu(nodes)
            
            if input_cmd == "s":
                self.save_settings(nodes)
            elif input_cmd == "q":
                break
            else:
                try:
                    # Parse input command to update a setting
                    input_cmd = input_cmd.split(" ")
                    if len(input_cmd) != 3:
                        print("Invalid command")
                        continue
                    node_id = int(input_cmd[0])
                    parameter_id = int(input_cmd[1])
                    new_value = float(input_cmd[2])
                except:
                    print("Invalid command")
                # Update nodes settings
                if node_id < 0 or node_id >= len(nodes):
                    print("Invalid node ID")
                    continue
                nodes[node_id].change_setting(parameter_id, new_value)
        self.db.close()          

    # List all nodes with their information
    def list_nodes(self):
        self.clear()
        self.db.connect()
        rows = self.db.query(query="SELECT * FROM nodes", val=None, fetchall=True)
        self.db.close()
        
        nodes = []
        for ip, name, resource, settings_json in rows:
            try:
                settings = json.loads(settings_json)
                for k in list(settings.keys()):
                    settings[k] = settings[k] / self.decimal_accuracy
            except (TypeError, json.JSONDecodeError):
                settings = settings_json  
            nodes.append([ip, name, resource, settings])

        headers = ["Ip", "Name", "Resource", "Parameters Setting"]
        print(tabulate(nodes, headers=headers, tablefmt="psql"))
        input("Press any key to return to menu...")

    # Clear the terminal screen
    def clear(self):
        os.system('cls' if os.name == 'nt' else 'clear')
