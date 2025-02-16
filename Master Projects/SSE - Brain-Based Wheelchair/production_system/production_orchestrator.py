"""
    Orchestrator of production system

    Author: Alessandro Ascani
"""
import json
import time
from production_system.configuration_parameters import ConfigurationParameters
from production_system.production_system_communication import ProductionSystemIO
from production_system.classification import Classification
from production_system.deployment import Deployment
from production_system.json_validation import JsonHandler







class ProductionOrchestrator:
    """
        Production system orchestrator.
    """
    def __init__(self, service: bool, unit_test:bool):

        self._service = service
        self._unit_test = unit_test


        self._configuration = ConfigurationParameters()
        self._evaluation_phase = self._configuration.parameters['evaluation_phase']
        self._prod_sys_io = ProductionSystemIO("0.0.0.0", 5005)
        self._session_counter = 0
        self._deployed = False




    def production(self):
        """
        Start production process.

        """
        print("Start production process")
        self._prod_sys_io.start_server()
        while True:

            # receive classifier or prepared session
            message = self._prod_sys_io.get_last_message()



            if self._service:
                print("Send start message to service class")
                self._prod_sys_io.send_timestamp(time.time(), "start")


            handler = JsonHandler()



            #develop session
            if message['ip'] == self._configuration.global_netconf['Development System']['ip'] :
                #deploy operation
                print("Classifier received")

                #convert json message in object class
                classifier_json = message['message']

                deployment = Deployment()
                result = deployment.deploy(classifier_json)
                if result is False:
                    print("error in classifier deployment")

                print("classifier deployed")
                self._deployed = True

                if self._service:
                    print("Send end message to Service Class")
                    self._prod_sys_io.send_timestamp(time.time(), "end")


                # send start configuration to messaging system
                print("Send start configuration")
                self._prod_sys_io.send_configuration()

                if self._unit_test:
                    return



            # classify session
            elif message['ip'] == self._configuration.global_netconf['Preparation System']['ip'] :
                #classify operation
                print("Prepared session received")
                prepared_session = message['message']
                prepared_session_dict = json.loads(prepared_session)
                # validation of json schema
                schemas_path = "production_schema/PreparedSessionSchema.json"
                result = handler.validate_json(prepared_session_dict, schemas_path)
                if result is False:
                    print("prepared session not valid")
                    break

                classification = Classification()
                label = classification.classify(prepared_session_dict, self._deployed)
                if label is None:
                    print("error: classifier not deployed")
                    break

                print("label generated")


                #if evaluation phase parameter is true label is sent also to Evaluation System
                if self._evaluation_phase:
                    eval_sys_ip = self._configuration.global_netconf['Evaluation System']['ip']
                    eval_sys_port = self._configuration.global_netconf['Evaluation System']['port']
                    print("Send label to evaluate session")
                    self._prod_sys_io.send_label(eval_sys_ip, eval_sys_port, label, "send")

                # Send label to client
                serv_cl_ip = self._configuration.global_netconf['Service Class']['ip']
                serv_cl_port = self._configuration.global_netconf['Service Class']['port']
                print("Send label to service class")
                self._prod_sys_io.send_label(serv_cl_ip, serv_cl_port, label, "client")
                self._session_counter += 1
                print(self._evaluation_phase)
                print(self._session_counter)

                if self._service:
                    print("Send end message to Service Class")
                    self._prod_sys_io.send_timestamp(time.time(), "end")


                if self._evaluation_phase is True and self._session_counter == self._configuration.parameters['max_session_evaluation']:
                    self._session_counter = 0
                    self._evaluation_phase = False

                elif self._evaluation_phase is False and self._session_counter == self._configuration.parameters['max_session_production']:
                    self._session_counter = 0
                    self._evaluation_phase = False

                if self._unit_test:
                    return

            else:
                print("sender unknown")
                if self._unit_test:
                    return



if __name__ == "__main__":

    production_system_orchestrator = ProductionOrchestrator(True, False)
    production_system_orchestrator.production()
