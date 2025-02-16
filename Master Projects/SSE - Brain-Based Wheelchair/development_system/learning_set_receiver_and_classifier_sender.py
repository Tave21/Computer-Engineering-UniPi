"""
Module: learning_set_receiver_and_classifier_sender
Represents the class to send and receive data.

Author: Gabriele Pianigiani

"""
import json

from flask import Flask, request, jsonify
import threading
import requests
from queue import Queue
from typing import Optional, Dict

from development_system.json_validator_reader_and_writer import JsonValidatorReaderAndWriter


class LearningSetReceiverAndClassifierSender:
    """A utility class to enable inter-module communication using Flask."""

    #This class supports sending and receiving messages in a blocking manner.

    def __init__(self, host: str = '0.0.0.0', port: int = 5000):
        """
        Initialize the Flask communication server.

        :param host: The host address for the Flask server.
        :param port: The port number for the Flask server.
        """
        self.app = Flask(__name__)
        self.host = host
        self.port = port
        self.json_handler = JsonValidatorReaderAndWriter()
        # Queue to hold received messages
        self.message_queue = Queue()

        # Define a route to receive messages
        @self.app.route('/send', methods=['POST'])
        def receive_message():
            data = request.json
            sender_ip = request.remote_addr
            sender_port = data.get('port')
            message = data.get('message')

            # Put the received message in the queue
            self.message_queue.put({
                'ip': sender_ip,
                'port': sender_port,
                'message': message
            })

            return jsonify("Development System: learning set received"), 200

    def start_server(self):
        """
        Start the Flask server in a separate thread.
        """
        thread = threading.Thread(target=self.app.run, kwargs={'host': self.host, 'port': self.port}, daemon=True)
        thread.start()

    def send_classifier(self, test=False) -> Optional[Dict]:
        """
        Send the winner classifier to the target module production system.
        :param test: Boolean which is True only to test the function locally
        :return: The response from the target, if any.
        """
        # Retrieve ip address and port of the target system
        if not test:
            classifier_file = "data/classifier.sav"
            self.json_handler.validate_json("conf/netconf.json", "schemas/netconf_schema.json")
            endpoint = self.json_handler.get_system_address("conf/netconf.json", "Production System")

            target_ip = endpoint["ip"]
            target_port = endpoint["port"]
        else:
            #this is true only for the local testing
            classifier_file = "data/mock_classifier.json"
            target_ip = "127.0.0.1"
            target_port = 5001

        with open(classifier_file, "rb") as f:
            file_content = f.read()
            message = file_content.decode('latin1')  # Encode binary content to a string format

        url = f"http://{target_ip}:{target_port}/send"
        payload = {
            "port": self.port,
            "message": message
        }
        try:
            response = requests.post(url, json=payload)
            if response.status_code == 200:
                return response.json()
        except requests.RequestException as e:
            print(f"Error sending message: {e}")
        except (UnicodeDecodeError, IOError) as e:
            print(f"Error processing file: {e}")
        return None

    def send_configuration(self, test=False) -> Optional[Dict]:
        """
        Send the configuration to the target module messaging system.
        :param test: Boolean which is True only to test the function locally
        :return: The response from the target, if any.
        """
        if not test:
            self.json_handler.validate_json("conf/netconf.json", "schemas/netconf_schema.json")
            endpoint = self.json_handler.get_system_address("conf/netconf.json", "Messaging System")

            target_ip = endpoint["ip"]
            target_port = endpoint["port"]
        else:
            # this is true only for the local testing
            target_ip = "127.0.0.1"
            target_port = 5001

        restart_config = {"action": "restart"}

        url = f"http://{target_ip}:{target_port}/MessagingSystem"

        payload = {
            "port": self.port,
            "message": restart_config
        }
        try:
            response = requests.post(url, json=payload)
            if response.status_code == 200:
                return response.json()
        except requests.RequestException as e:
            print(f"Error sending message: {e}")

        return None

    def rcv_learning_set(self) -> Optional[Dict]:
        """
        Wait for a message to be received and return it.

        :return: A dictionary containing the sender's IP, port, and the message content.
        """
        # Block until a message is available in the queue
        message = self.message_queue.get()
        return message

    def send_timestamp(self, timestamp: float, status: str) -> bool:
        """
        Send the timestamp to the Service Class.

        :param timestamp: The timestamp to send.
        :param status: The status of the timestamp
        :return: True if the timestamp was sent successfully, False otherwise.
        """
        # Retrieve ip address and port of the target system
        self.json_handler.validate_json("conf/netconf.json", "schemas/netconf_schema.json")
        endpoint = self.json_handler.get_system_address("conf/netconf.json", "Service Class")
        target_ip = endpoint["ip"]
        target_port = endpoint["port"]

        url = f"http://{target_ip}:{target_port}/Timestamp"

        timestamp_message = {
            "timestamp": timestamp,
            "system": "Development System",
            "status": status
        }

        packet = {
            "port": 5004,
            "message": json.dumps(timestamp_message)
        }

        try:
            response = requests.post(url, json=packet)
            if response.status_code == 200:
                return True
        except requests.RequestException as e:
            print(f"Error sending timestamp: {e}")
        return False