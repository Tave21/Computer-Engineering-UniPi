"""
Module: test_ingestion_orchestrator
Test the ingestion system workflow.
Author: Francesco Taverna

"""

import json
import logging
from multiprocessing import Process
import time

from clientSideSystem.client import ClientSideOrchestrator
from ingestion_system.ingestionSystemTest.MessageBrokerTest import MessageBrokerTest
from ingestion_system.ingestion_system_orchestrator import IngestionSystemOrchestrator

logger = logging.getLogger()
logger.level = logging.INFO

def run_orchestrator():
    orchestrator = IngestionSystemOrchestrator()
    orchestrator.ingestion()


def run_client():
    client = ClientSideOrchestrator("dataTest/")
    client.run(5, True)


def test_ingestion_system_orchestrator():
    # create receiver
    receiver = MessageBrokerTest(host='127.0.0.1', port=5012)
    #set current_phase as "evaluation" in the config file to test also this sending
    label_receiver = MessageBrokerTest(host='127.0.0.1', port=5013)
    receiver.start_server()
    label_receiver.start_server()

    # Run the orchestrator
    ingestion_system = Process(target=run_orchestrator, args=())
    ingestion_system.start()

    time.sleep(1)

    # Run client
    client_system = Process(target=run_client)
    client_system.start()

    raw_sessions = []
    labels = []
    num_sessions = 14
    num_labels = 14
    # waits for the sessions
    for i in range(num_sessions):
        message = receiver.get_message()
        print("ricevo : ", i)
        raw_session = json.loads(message['message']) #convert to dictionary
        raw_sessions.append(raw_session)
    # waits for labels
    for i in range(num_labels):
        print("ricevo labels : ", i)
        message = label_receiver.get_message()
        label = json.loads(message['message'])
        labels.append(label)

    ingestion_system.terminate()
    client_system.terminate()
    assert len(raw_sessions) == num_sessions
    assert len(labels) == num_labels

if __name__ == "__main__":
    test_ingestion_system_orchestrator()
