"""
Module: ingestion_system_orchestrator
Orchestrates the ingestion system workflow.

Author: Francesco Taverna

"""
import json

from ingestion_system.record_buffer_controller import RecordBufferController
from ingestion_system.raw_session_preparation import RawSessionPreparation
from ingestion_system.ingestion_system_parameters import Parameters
from ingestion_system.SessionAndRecordExchanger import SessionAndRecordExchanger


class IngestionSystemOrchestrator:
    """
    Orchestrator for the ingestion system workflow.
    Manages instances of system components.
    """

    def __init__(self):
        """
        Initializes the IngestionSystemOrchestrator object.


        Example:
            orchestrator = IngestionSystemOrchestrator()
        """

        print("INGESTION ORCHESTRATOR INITIALIZATION")

        # parameters class configuration
        self.parameters = Parameters()

        # buffer class configuration
        self.buffer_controller = RecordBufferController()

        # raw session configuration
        self.session_preparation = RawSessionPreparation()

        # IO configuration
        self.json_io = SessionAndRecordExchanger(host= self.parameters.configuration["ip_ingestion"]
                                                 , port=self.parameters.configuration["port_ingestion"])  # parameters of Ingestion server
        self.json_io.start_server()
        self.number_of_missing_samples = 0
        self.current_phase = self.parameters.configuration["current_phase"]

        self.current_sessions = 0

        print("INGESTION ORCHESTRATOR INITIALIZED")

    def _update_session(self):
        # updates the number of session received and eventually changes the current phase
        self.current_sessions += 1

        #if we are in production and the number of sessions sent is reached, change to evaluation
        if self.current_phase == "production" and self.current_sessions == self.parameters.configuration["production_sessions"]:
            self.current_phase = "evaluation"
            self.current_sessions = 0
            print("CHANGED TO EVALUATION")
        # if we are in evaluation and the number of sessions sent is reached, change to production
        elif self.current_phase == "evaluation" and self.current_sessions == self.parameters.configuration["evaluation_sessions"]:
            self.current_phase = "production"
            self.current_sessions = 0
            print("CHANGED TO PRODUCTION")


    def ingestion(self):
        """
        Process a record through the ingestion workflow.
        """
        i = 1
        j = 1
        while True:  # receive records iteratively
            try:
                print("i-esimo record: ", i)
                i = i+ 1
                #boo is True if the message doesn't have the correct record schema
                boo, new_record = self.json_io.get_message()
                if boo:
                    continue

                # stores record
                self.buffer_controller.store_record(new_record)
                print(new_record)

                # retrieve records
                stored_records = self.buffer_controller.get_records(new_record["value"]["UUID"])

                # if there is at least one None: not enough records
                if None in stored_records:
                    #if it is not production, so it is development or evaluation, wait for label and others
                    #or if there are at least two None, so not only the label is missing, but others, wait for them
                    #in this way, if it is production phase and there is only one None, it means that the label is
                    #missing and given that in production is not required, the raw session is completed
                    if self.current_phase != "production" or stored_records.count(None) >= 2:
                        continue


                # creates raw session
                raw_session = self.session_preparation.create_raw_session(stored_records)
                print("sto mandando la raw session: ", raw_session.to_json())
                print("numero di raw session inviata: ", j)
                j = j + 1

                # removes records
                self.buffer_controller.remove_records(new_record["value"]["UUID"])

                # marks missing samples with "None" and checks the number
                self.number_of_missing_samples, marked_raw_session = self.session_preparation.mark_missing_samples(
                    raw_session, None)
                if self.number_of_missing_samples >= self.parameters.configuration["missing_samples_threshold_interval"] :
                    continue  # do not send anything

                # if in evaluation phase, sends labels to evaluation system
                if self.current_phase == "evaluation":
                    label = {
                        "uuid": marked_raw_session.uuid,
                        "movements": marked_raw_session.label
                    }
                    json_label = json.dumps(label) #json
                    print("invio label a gio: ", json_label)

                    self.json_io.send_message(target_ip=self.parameters.configuration["ip_evaluation"],
                                              target_port=self.parameters.configuration["port_evaluation"], message=json_label)

                # sends raw sessions
                json_raw_session = marked_raw_session.to_json()
                self.json_io.send_message(target_ip=self.parameters.configuration["ip_preparation"],
                                          target_port=self.parameters.configuration["port_preparation"], message=json_raw_session)

                #update the session sent counter only it is production/evaluation
                #because development is changed by the human
                if self.current_phase != "development":
                    #if it is not production testing phase, count sessions to change then phase
                    #otherwise, if it is production testing, stop counting, no phase change needed
                    if not self.parameters.configuration["service"]:
                        self._update_session()

            except Exception as e:
                print(f"Error during ingestion: {e}")




if __name__ == "__main__":
    orchestrator = IngestionSystemOrchestrator()
    orchestrator.ingestion()


