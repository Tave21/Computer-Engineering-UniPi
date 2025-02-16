"""
Module: test_preparation_orchestrator
Test the preparation system workflow.
Author: Francesco Taverna
"""

import json
import logging
from multiprocessing import Process
import time

from clientSideSystem.client import ClientSideOrchestrator
from ingestion_system.ingestion_system_orchestrator import IngestionSystemOrchestrator
from preparation_system.PreparationSystemOrchestrator import PreparationSystemOrchestrator
from preparation_system.PreparationSystemParameters import PreparationSystemParameters
from preparation_system.PreparationSystemTest.MessageBrokerTest import MessageBrokerTest

logger = logging.getLogger()
logger.level = logging.INFO




def run_ingestion():
    orchestrator = IngestionSystemOrchestrator()
    orchestrator.ingestion()


def run_client():
    client = ClientSideOrchestrator("../../clientSideSystem/clientSideSystemTest/dataTest/")
    client.run(5, True)


def run_preparation():
    orchestrator = PreparationSystemOrchestrator()
    orchestrator.run()


def test_preparation_system_orchestrator():
    # create receiver
    #set "development" as True in the config file to test this receive
    receiver_segregation = MessageBrokerTest(host='127.0.0.1', port=5041)
    receiver_segregation.start_server()
    #set "development" as False in the config file to test this receive
    receiver_production = MessageBrokerTest(host='127.0.0.1', port=5045)
    receiver_production.start_server()
    parameters = PreparationSystemParameters()

    preparation_system = Process(target=run_preparation, args=())
    preparation_system.start()

    # Run ingestion
    ingestion_system = Process(target=run_ingestion, args=())
    ingestion_system.start()

    time.sleep(1)

    # Run client
    client_system = Process(target=run_client)
    client_system.start()

    prep_sessions_segregation = []
    prep_sessions_production = []
    num_sessions_segregation = 14
    num_sessions_production = 14
    # waits for the sessions
    if parameters.configuration["development"]:
        for i in range(num_sessions_segregation):
            message = receiver_segregation.get_message()
            print("ricevo : ", i)
            prep_session = json.loads(message["message"])
            prep_sessions_segregation.append(prep_session)
    else:
        for i in range(num_sessions_production):
            message = receiver_production.get_message()
            print("ricevo production : ", i)
            prep_session = json.loads(message["message"])
            prep_sessions_production.append(prep_session)

    # terminates
    preparation_system.terminate()
    ingestion_system.terminate()
    client_system.terminate()
    if not parameters.configuration["development"]:
        assert len(prep_sessions_production) == num_sessions_production
    else:
        assert len(prep_sessions_segregation) == num_sessions_segregation

if __name__ == "__main__":
    test_preparation_system_orchestrator()
