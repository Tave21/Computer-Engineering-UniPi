"""

Module: PreparationSystemOrchestrator
Orchestrates the preparation system workflow.

Author: Francesco Taverna
"""
import json
from asyncio.subprocess import Process

from preparation_system.PreparationSystemParameters import PreparationSystemParameters
from preparation_system.RawSessionReceiver_and_PreparedSessionSender import RawSessionReceiver_and_PrepareSessionSender
from preparation_system.SessionPreparation import SessionPreparation


class PreparationSystemOrchestrator:
    def __init__(self):
        print("-- PREPARATION SYSTEM STARTED --")


        self.parameters = PreparationSystemParameters()

        #instantiate message exchanger
        self.communication = RawSessionReceiver_and_PrepareSessionSender(host=self.parameters.configuration["ip_preparation"],
                                                                         port=self.parameters.configuration["port_preparation"])
        self.communication.start_server()
        #prepare SessionPreparation istance with all configuration parameters
        self.session_preparation = SessionPreparation()

        print("-- PREPARATION SYSTEM INITIALIZED --")

    def run(self) -> None:
        while True:
            # receive message
            boo, new_raw_session = self.communication.get_message()
            print("preparation ricevuta raw session: ", new_raw_session)
            if boo:
                continue
            # correct raw session
            raw_session_corrected = self.session_preparation.correct_missing_samples(new_raw_session, None)
            raw_session_corrected = self.session_preparation.correct_outliers(raw_session_corrected)

            # create prepared session
            prepared_session = self.session_preparation.create_prepared_session(raw_session_corrected)
            json_prepared_session = json.dumps(prepared_session)

            # send prepared session
            if self.parameters.configuration["development"]:
                print("INVIO A SAVE")
                #send to segregation system
                self.communication.send_message(self.parameters.configuration["ip_segregation"],
                                                self.parameters.configuration["port_segregation"], json_prepared_session)
            else:
                """
                data = json.loads(json_prepared_session)
                
                data.pop("label", None)  
                
                json_prepared_session = json.dumps(data)
                """

                print("INVIO A ALE")
                self.communication.send_message(self.parameters.configuration["ip_production"],
                                                self.parameters.configuration["port_production"], json_prepared_session)



if __name__ == "__main__":
    orchestrator = PreparationSystemOrchestrator()
    orchestrator.run()
