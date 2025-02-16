import threading
import time
import unittest

from message_broker import MessageBroker


class TestMessageBroker(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        """
        Setup two MessageBroker instances on different ports for testing communication.
        """
        cls.module_a = MessageBroker(host='127.0.0.1', port=5001)
        cls.module_b = MessageBroker(host='127.0.0.1', port=5002)

        # Start both servers in separate threads
        threading.Thread(target=cls.module_a.start_server, daemon=True).start()
        threading.Thread(target=cls.module_b.start_server, daemon=True).start()

        # Allow some time for the servers to start
        time.sleep(1)

    def test_send_and_receive_message(self):
        """
        Test that a message sent from module_a is received by module_b.
        """
        # Send a message from module_a to module_b
        message = {"action": "test_action", "payload": "test_payload"}
        response = self.module_a.send_message(target_ip=self.module_b.host, target_port=self.module_b.port, message=str(message))

        # Check that module_b acknowledges receipt
        self.assertIsNotNone(response)
        self.assertEqual(response.get("status"), "received")

        # Retrieve the message received by module_b
        received_message = self.module_b.get_last_message()
        self.assertIsNotNone(received_message)
        self.assertEqual(received_message['message'], str(message))
        self.assertEqual(received_message['ip'], self.module_a.host)  # IP address of sender
        self.assertEqual(received_message['port'], self.module_a.port)  # Port of module_a

    def test_no_message_received(self):
        """
        Test that get_last_message blocks until a message is received.
        """
        def delayed_send():
            time.sleep(2)  # Delay sending a message
            self.module_a.send_message(target_ip=self.module_b.host, target_port=self.module_b.port, message="{\"action\": \"delayed_test\"}")

        # Start a thread to send a delayed message
        threading.Thread(target=delayed_send, daemon=True).start()

        # Call get_last_message and ensure it blocks until the message is received
        start_time = time.time()
        received_message = self.module_b.get_last_message()
        elapsed_time = time.time() - start_time

        # Check that it took at least 2 seconds (blocking wait)
        self.assertGreaterEqual(elapsed_time, 2)

        # Verify the received message
        self.assertIsNotNone(received_message)
        self.assertEqual(received_message['message'], '{"action": "delayed_test"}')

    @classmethod
    def tearDownClass(cls):
        """
        Clean up any resources if necessary.
        """
        # No specific cleanup is required, as servers are run in daemon threads
        pass

if __name__ == '__main__':
    unittest.main()
