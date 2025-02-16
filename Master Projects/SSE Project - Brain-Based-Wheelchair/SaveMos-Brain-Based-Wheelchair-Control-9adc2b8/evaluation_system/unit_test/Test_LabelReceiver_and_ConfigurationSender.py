import json
import unittest
from unittest.mock import patch, MagicMock
import jsonschema
from evaluation_system.LabelReceiver_and_ConfigurationSender import LabelReceiver_and_ConfigurationSender
from evaluation_system.EvaluationSystemParameters import EvaluationSystemParameters
from evaluation_system.Label import Label

class TestLabelReceiverAndConfigurationSender(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        """Setup the Flask app and the LabelReceiverAndConfigurationSender instance"""
        # Load parameters first, before running any tests
        EvaluationSystemParameters.loadParameters("..")
        cls.app = LabelReceiver_and_ConfigurationSender(host="127.0.0.1", port=5000, basedir="..")

    @patch('requests.post')
    def test_send_configuration_success(self, mock_post):
        """Test if the send_configuration method successfully sends the configuration"""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_post.return_value = mock_response

        result = self.app.send_configuration()
        self.assertTrue(result)
        mock_post.assert_called_once()

    @patch('requests.post')
    def test_send_configuration_failure(self, mock_post):
        """Test if the send_configuration method fails to send the configuration"""
        mock_response = MagicMock()
        mock_response.status_code = 500  # Simulating failure
        mock_post.return_value = mock_response

        result = self.app.send_configuration()
        self.assertFalse(result)
        mock_post.assert_called_once()

    @patch('requests.post')
    def test_send_timestamp_success(self, mock_post):
        """Test if the send_timestamp method successfully sends the timestamp"""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_post.return_value = mock_response

        timestamp = 1638387362.5
        phase = 'test_phase'
        result = self.app.send_timestamp(timestamp, phase)
        self.assertTrue(result)
        mock_post.assert_called_once()

    @patch('requests.post')
    def test_send_timestamp_failure(self, mock_post):
        """Test if the send_timestamp method fails to send the timestamp"""
        mock_response = MagicMock()
        mock_response.status_code = 500  # Simulating failure
        mock_post.return_value = mock_response

        timestamp = 1638387362.5
        phase = 'test_phase'
        result = self.app.send_timestamp(timestamp, phase)
        self.assertFalse(result)
        mock_post.assert_called_once()

    @patch('jsonschema.validate')
    def test_validate_json_label_valid(self, mock_validate):
        """Test if the JSON label is correctly validated when it is valid"""
        valid_json_label = {
            "uuid": "1234-5678-9012",
            "movements": "turnRight"
        }
        mock_validate.return_value = None  # Mock successful validation

        result = self.app._validate_json_label(valid_json_label)
        self.assertTrue(result)

    @patch('jsonschema.validate')
    def test_validate_json_label_invalid(self, mock_validate):
        """Test if the JSON label is correctly validated when it is invalid"""
        invalid_json_label = {
            "uuid": "1234-5678-9012"
            # Missing "movements"
        }
        mock_validate.side_effect = jsonschema.ValidationError("Invalid JSON label")

        result = self.app._validate_json_label(invalid_json_label)
        self.assertFalse(result)


    def test_receive_label_valid(self):
        """
        Test if the /send route correctly handles a valid label.
        Sender IP can be either the Ingestion System or the Production System.
        """
        valid_json_label = {
            "uuid": "1234-5678-9012",
            "movements": "move"
        }

        with self.app.app.test_client() as client:

            # Preparing the packet of the Ingestion System
            ingestion_packet = {
                "port": EvaluationSystemParameters.GLOBAL_PARAMETERS["Ingestion System"]["port"],
                "message": json.dumps(valid_json_label)
            }

            # Simulate a request from the Ingestion System
            response = client.post('/send',
                                   json=ingestion_packet,
                                   environ_base={"REMOTE_ADDR": EvaluationSystemParameters.GLOBAL_PARAMETERS["Ingestion System"]["ip"]})
            self.assertEqual(response.status_code, 200)
            self.assertEqual(response.json, {"status": "received"})
            label = self.app.get_label()
            self.assertIsInstance(label, Label)
            self.assertEqual(label.uuid, valid_json_label['uuid'])
            self.assertEqual(label.movements, valid_json_label['movements'])
            self.assertTrue(label.expert)

            # Preparing the packet of the Production System
            production_packet = {
                "port": EvaluationSystemParameters.GLOBAL_PARAMETERS["Production System"]["port"],
                "message": json.dumps(valid_json_label)
            }

            # Simulate a request from the Production System
            response = client.post('/send',
                                   json=production_packet,
                                   environ_base={"REMOTE_ADDR": EvaluationSystemParameters.GLOBAL_PARAMETERS["Production System"]["ip"]})
            self.assertEqual(response.status_code, 200)
            self.assertEqual(response.json, {"status": "received"})
            label = self.app.get_label()
            self.assertIsInstance(label, Label)
            self.assertEqual(label.uuid, valid_json_label['uuid'])
            self.assertEqual(label.movements, valid_json_label['movements'])
            self.assertFalse(label.expert)

    def test_receive_label_invalid(self):
        """Test if the /send route correctly handles an invalid label"""
        with self.app.app.test_client() as client:
            invalid_json_label = {
                "uuid": "1234-5678-9012"
                # Missing "movements"
            }

            # Preparing the packet
            packet = {
                "port": self.app.port,
                "message": json.dumps(invalid_json_label)
            }

            response = client.post('/send', json=packet)
            self.assertEqual(response.status_code, 400)
            self.assertEqual(response.json, {"status": "error", "message": "Invalid JSON label"})

    def test_receive_label_invalid_ip(self):
        """Test if the /send route rejects an invalid sender IP."""
        valid_json_label = {
            "uuid": "1234-5678-9012",
            "movements": "turnLeft"
        }

        # Patch _validate_json_label to always return True for this test
        with patch.object(self.app, '_validate_json_label', return_value=True):
            with self.app.app.test_client() as client:

                # Preparing the packet
                packet = {
                    "port": self.app.port,
                    "message": json.dumps(valid_json_label)
                }

                # Simulate a request with an invalid IP address
                response = client.post('/send',
                                       json=packet,
                                       environ_base={"REMOTE_ADDR": "invalid_ip"})

                # Assertions for invalid IP
                self.assertEqual(response.status_code, 400)
                self.assertEqual(response.json, {"status": "error", "message": "Invalid sender IP"})

    @patch('requests.post')
    def test_receive_label_expert(self, mock_post):
        """Test if the /send route correctly identifies an expert label"""
        valid_json_label = {
            "uuid": "1234-5678-9012",
            "movements": "move"
        }

        with patch.object(self.app, '_validate_json_label', return_value=True):
            with self.app.app.test_client() as client:

                # Preparing the packet
                packet = {
                    "port": self.app.port,
                    "message": json.dumps(valid_json_label)
                }

                # Simulate a request from the Ingestion System
                response = client.post('/send',
                                       json=packet,
                                       environ_base={"REMOTE_ADDR": EvaluationSystemParameters.GLOBAL_PARAMETERS["Ingestion System"]["ip"]})
                self.assertEqual(response.status_code, 200)
                self.assertEqual(response.json, {"status": "received"})
                label = self.app.get_label()
                self.assertIsInstance(label, Label)
                self.assertEqual(label.uuid, valid_json_label['uuid'])
                self.assertEqual(label.movements, valid_json_label['movements'])
                self.assertTrue(label.expert)

    def test_get_label(self):
        """Test if get_label returns a Label object"""
        valid_json_label = {
            "uuid": "1234-5678-9012",
            "movements": "move"
        }
        # Push the label into the queue
        label = Label(uuid=valid_json_label['uuid'], movements=valid_json_label['movements'], expert=True)
        self.app.label_queue.put(label)

        result = self.app.get_label()
        self.assertEqual(result._uuid, label._uuid)
        self.assertEqual(result._movements, label._movements)
        self.assertTrue(result._expert)


if __name__ == '__main__':
    unittest.main()
