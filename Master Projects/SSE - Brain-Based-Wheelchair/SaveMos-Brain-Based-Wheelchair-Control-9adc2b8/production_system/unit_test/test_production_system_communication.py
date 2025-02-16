"""
Author: Alessandro Ascani
"""

import unittest
from unittest.mock import patch, MagicMock
from production_system.production_system_communication import ProductionSystemIO
from production_system.label import Label
from production_system.configuration_parameters import ConfigurationParameters

class TestProductionSystemIO(unittest.TestCase):

    def setUp(self):
        """
        Set up a ProductionSystemIO instance for testing.
        """
        self.system_io = ProductionSystemIO(host='127.0.0.1', port=5001)

    @patch('production_system.production_system_communication.requests.post')
    def test_send_configuration(self, mock_post):
        """
        Test the send_configuration method.
        """
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {"status": "ok"}

        with patch.object(ConfigurationParameters, 'MESSAGING_SYSTEM_IP', '127.0.0.1'), \
             patch.object(ConfigurationParameters, 'MESSAGING_SYSTEM_PORT', 5002):

            response = self.system_io.send_configuration(message="Test Message")

        self.assertEqual(response, {"status": "ok"})
        mock_post.assert_called_once_with(
            'http://127.0.0.1:5002/send',
            json={"port": 5001, "message": "Test Message"}
        )

    @patch('production_system.production_system_communication.requests.post')
    def test_send_label(self, mock_post):
        """
        Test the send_label method.
        """
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {"status": "received"}

        label = MagicMock()
        label.to_dictionary.return_value = {"type": "test", "value": "example"}

        response = self.system_io.send_label(
            target_ip="127.0.0.1",
            target_port=5003,
            label=label
        )

        self.assertEqual(response, {"status": "received"})
        mock_post.assert_called_once_with(
            'http://127.0.0.1:5003/send',
            json={"port": 5001, "message": '{"type": "test", "value": "example"}'}
        )

    def test_get_last_message(self):
        """
        Test the get_last_message method.
        """
        with self.system_io.message_condition:
            self.system_io.last_message = {
                'ip': '127.0.0.1',
                'port': 5004,
                'message': 'Hello World'
            }
            self.system_io.message_condition.notify_all()

        result = self.system_io.get_last_message()

        self.assertEqual(result, {
            'ip': '127.0.0.1',
            'port': 5004,
            'message': 'Hello World'
        })
        self.assertIsNone(self.system_io.last_message)

if __name__ == '__main__':
    unittest.main()
