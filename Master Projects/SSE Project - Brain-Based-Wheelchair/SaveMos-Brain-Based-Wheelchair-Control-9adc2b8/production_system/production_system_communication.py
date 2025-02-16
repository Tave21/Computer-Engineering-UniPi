"""
    Class for managing the sending and receiving of messages
"""
import queue
import threading
import requests
import json
from flask import Flask, request, jsonify
from typing import Optional, Dict
from production_system.label import Label
from production_system.configuration_parameters import ConfigurationParameters


class ProductionSystemIO:
    """

        this class manage all sent/received json file
    """

    def __init__(self, host: str = '0.0.0.0', port: int = 5005):
        """
        Initialize the Flask communication server.

        :param host: The host address for the Flask server.
        :param port: The port number for the Flask server.
        """
        self.app = Flask(__name__)
        self.host = host
        self.port = port
        self.msg_queue = queue.Queue()

        # Lock and condition for blocking behavior
        self.message_condition = threading.Condition()

        # Define a route to receive messages
        @self.app.route('/send', methods=['POST'])
        def receive_message():
            # Get the sender's IP

            data = request.json
            sender_ip = request.remote_addr
            sender_port = data.get('port')
            message_content = data.get('message')

            message = {
                'ip': sender_ip,
                'port': sender_port,
                'message': message_content
            }

            self.msg_queue.put(message)

            return jsonify({"status": "received"}), 200

    def start_server(self):
        """
        Start the Flask server in a separate thread.
        """
        thread = threading.Thread(target=self.app.run, kwargs={'host': self.host, 'port': self.port}, daemon=True)
        thread.start()

    def send_configuration(self) -> Optional[Dict]:
        """
        Send start configuration to messaging system.

        :return: The response from the target, if any.
        """

        # recover messaging system information
        configuration = ConfigurationParameters()
        message = configuration.start_config()
        msg_sys_ip = configuration.global_netconf['Messaging System']['ip']
        msg_sys_port = configuration.global_netconf['Messaging System']['port']
        url = f"http://{msg_sys_ip}:{msg_sys_port}/MessagingSystem"
        payload = {
            "port": self.port,
            "message": json.dumps(message)
        }
        try:
            response = requests.post(url, json=payload)
            if response.status_code == 200:
                return response.json()
        except requests.RequestException as e:
            print(f"Error sending message: {e}")
        return None

    def send_label(self, target_ip: str, target_port: int, label: [Dict], rule: str) -> Optional[Dict]:
        """
        Send a message to a target module.

        :param target_ip: The IP address of the target module.
        :param target_port: The port of the target module.
        :param label: The label to send.
        :return: The response from the target, if any.
        """

        # convert label into json
        label_dict = label.to_dictionary()

        label_json = json.dumps(label_dict)
        if rule == "send":
            url = f"http://{target_ip}:{target_port}/send"
        elif rule == "client":
            url = f"http://{target_ip}:{target_port}/ClientSide"
        payload = {
            "port": self.port,
            "message": label_json
        }
        try:
            response = requests.post(url, json=payload)
            if response.status_code == 200:
                return response.json()
        except requests.RequestException as e:
            print(f"Error sending message: {e}")
        return None

    def get_last_message(self) -> Optional[Dict]:
        """
       Wait for a message to be received and return it.

        :return: A dictionary containing the sender's IP, port, and the message content.
        """
        print("waiting new message...")
        return self.msg_queue.get(block=True)

    # Testing method
    def send_timestamp(self, timestamp: float, status: str) -> bool:
        """
        Send the timestamp to the Service Class.

        :param timestamp: The timestamp to send.
        :param status: The status of the timestamp
        :return: True if the timestamp was sent successfully, False otherwise.
        """

        configuration = ConfigurationParameters()
        service_ip = configuration.global_netconf['Service Class']['ip']
        service_port = configuration.global_netconf['Service Class']['port']
        url = f"http://{service_ip}:{service_port}/Timestamp"

        timestamp_message = {
            "timestamp": timestamp,
            "system": "Production System",
            "status": status
        }

        try:

            # Preparing the packet to send
            packet = {
                "port": configuration.global_netconf["Production System"]["port"],
                "message": json.dumps(timestamp_message)
            }

            response = requests.post(url, json=packet)
            if response.status_code == 200:
                return True
        except requests.RequestException as e:
            print(f"Error sending timestamp: {e}")
        return False
