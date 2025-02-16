"""
Module: message_broker_test
Tests the function to send and receive data from other modules.

Author: Gabriele Pianigiani

"""
import threading

from development_system.learning_set_receiver_and_classifier_sender import LearningSetReceiverAndClassifierSender


class TestMessageBroker:
    @staticmethod
    def receive_message():
        """Receive messages using a broker instance."""
        # Create the main broker instance on port 5001
        broker_5001 = LearningSetReceiverAndClassifierSender(port=5001)
        broker_5001.start_server()

        print("Receiver instance created, waiting for messages...")

        messages = []
        while len(messages) < 2:
            message = broker_5001.rcv_learning_set()
            print(f"Received message: {message}\n")
            messages.append(message)

        print("All messages received.")

    @staticmethod
    def send_classifier():
        """Send a classifier to a specific host and port."""
        # Create the first sender instance on port 5002
        classifier_sender = LearningSetReceiverAndClassifierSender(host='127.0.0.1', port=5002)
        classifier_sender.start_server()

        classifier_file = "data/mock_classifier.json"
        with open(classifier_file, "w") as file:
            file.write("{\"classifier\": \"MockClassifier\"}")

        print("Sending classifier...")
        response = classifier_sender.send_classifier(True)
        print(f"send_classifier response: {response}")

    @staticmethod
    def send_configuration():
        """Send a configuration to a specific host and port."""
        # Create the second sender instance on port 5003
        configuration_sender = LearningSetReceiverAndClassifierSender(host='127.0.0.1', port=5003)
        configuration_sender.start_server()

        print("Sending configuration...")
        response = configuration_sender.send_configuration(True)
        print(f"send_configuration response: {response}")


if __name__ == "__main__":
    # Create threads for each method, they are necessary otherwise the system is blocked when
    #the receive_message function is called waiting for the messages and the sender can't send message
    receiver_thread = threading.Thread(target=TestMessageBroker.receive_message, daemon=True)
    classifier_thread = threading.Thread(target=TestMessageBroker.send_classifier, daemon=True)
    configuration_thread = threading.Thread(target=TestMessageBroker.send_configuration, daemon=True)

    # Start threads
    receiver_thread.start()

    # Delay to ensure the receiver server is running before sending messages
    threading.Event().wait(1)

    classifier_thread.start()
    configuration_thread.start()

    # Wait for threads to complete
    receiver_thread.join()
    classifier_thread.join()
    configuration_thread.join()

    print("Test completed.")
