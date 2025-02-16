"""
Author: Giovanni Ligato
"""


from typing import Optional, Dict
import json
import queue
import threading
import requests
import jsonschema
from flask import Flask, request, jsonify

from evaluation_system.EvaluationSystemParameters import EvaluationSystemParameters
from evaluation_system.Label import Label


class LabelReceiver_and_ConfigurationSender:
    """
    Evaluation System module responsible for receiving labels and sending configuration.
    """

    def __init__(self, host: str = '0.0.0.0', port: int = None, basedir: str = "."):
        """
        Initialize the Flask communication server.

        :param host: The host address for the Flask server.
        :param port: The port number for the Flask server.
        :param basedir: The base directory for the Flask server.
        """

        if port is None:
            port = EvaluationSystemParameters.GLOBAL_PARAMETERS["Evaluation System"]["port"]

        self.app = Flask(__name__)
        self.host = host
        self.port = port
        # Queue to store received labels
        self.label_queue = queue.Queue()

        # Path of the JSON schema for the labels
        self.label_schema_path = f"{basedir}/schemas/label_schema.json"

        # Define a route to receive labels
        @self.app.route('/send', methods=['POST'])
        def receive_label():

            # Get the sender's IP
            sender_ip = request.remote_addr

            packet = request.get_json()

            # Get the label from the packet
            json_label = json.loads(packet["message"])

            # Validate the label
            if self._validate_json_label(json_label):
                # JSON label is valid

                # Check if the label is an expert label (coming from the Ingestion System)
                # or a classifier label (coming from the Production System)
                if EvaluationSystemParameters.GLOBAL_PARAMETERS["Ingestion System"]["ip"] == sender_ip:
                    expert = True
                elif EvaluationSystemParameters.GLOBAL_PARAMETERS["Production System"]["ip"] == sender_ip:
                    expert = False
                else:
                    return jsonify({"status": "error", "message": "Invalid sender IP"}), 400

                # Create a Label object from the JSON label
                label = Label(uuid=json_label['uuid'], movements=json_label['movements'], expert=expert)
                self.label_queue.put(label)

                return jsonify({"status": "received"}), 200
            else:
                # JSON label is invalid
                return jsonify({"status": "error", "message": "Invalid JSON label"}), 400

    def start_server(self):
        """
        Start the Flask server in a separate thread.
        """
        thread = threading.Thread(target=self.app.run, kwargs={'host': self.host, 'port': self.port}, daemon=True)
        thread.start()

    def _validate_json_label(self, json_label: Dict) -> bool:
        """
        Validate a JSON label received from a sender.

        :param json_label: The JSON label to validate.
        """

        with open(self.label_schema_path, "r") as schema_file:
            label_schema = json.load(schema_file)

        try:
            jsonschema.validate(json_label, label_schema)
            return True
        except jsonschema.ValidationError as e:
            print(f"Invalid JSON label: {e}")
            return False

    def send_configuration(self) -> bool:
        """
        Send the configuration "restart" message to the Messaging System.

        :return: True if the message was sent successfully, False otherwise.
        """

        url = f"http://{EvaluationSystemParameters.GLOBAL_PARAMETERS['Messaging System']['ip']}:{EvaluationSystemParameters.GLOBAL_PARAMETERS['Messaging System']['port']}/MessagingSystem"

        configuration = {
            "configuration": "restart"
        }

        try:

            # Preparing the packet to send
            packet = {
                "port": EvaluationSystemParameters.GLOBAL_PARAMETERS["Evaluation System"]["port"],
                "message": json.dumps(configuration)
            }

            response = requests.post(url, json=packet)
            if response.status_code == 200:
                return True
        except requests.RequestException as e:
            print(f"Error sending configuration: {e}")
        return False


    def get_label(self) -> Optional[Label]:
        """
        Get the last label received by the server. Blocks until a label is available.

        :return: A Label object containing the UUID, movements, and expert fields.
        """
        return self.label_queue.get(block=True)


    # Testing method
    def send_timestamp(self, timestamp: float, status: str) -> bool:
        """
        Send the timestamp to the Service Class.

        :param timestamp: The timestamp to send.
        :param status: The status of the timestamp
        :return: True if the timestamp was sent successfully, False otherwise.
        """
        url = f"http://{EvaluationSystemParameters.GLOBAL_PARAMETERS['Service Class']['ip']}:{EvaluationSystemParameters.GLOBAL_PARAMETERS['Service Class']['port']}/Timestamp"

        timestamp_message = {
            "timestamp": timestamp,
            "system": "Evaluation System",
            "status": status
        }

        try:

            # Preparing the packet to send
            packet = {
                "port": EvaluationSystemParameters.GLOBAL_PARAMETERS["Evaluation System"]["port"],
                "message": json.dumps(timestamp_message)
            }

            response = requests.post(url, json=packet)
            if response.status_code == 200:
                return True
        except requests.RequestException as e:
            print(f"Error sending timestamp: {e}")
        return False
