"""
Author: Giovanni Ligato
"""

import time

from service_class.ServiceClassParameters import ServiceClassParameters
from service_class.ServiceReceiver import ServiceReceiver
from service_class.RecordSender import RecordSender
from service_class.CSVLogger import CSVLogger

class ServiceClassOrchestrator:
    """
    Service Class Orchestrator module responsible for managing the Service Class.
    """


    def __init__(self, basedir: str = "."):
        """
        Initialize the Service Class Orchestrator.

        :param basedir: The base directory for the Service Class Orchestrator.
        """

        self.basedir = basedir

        # Load the parameters of the Service Class
        ServiceClassParameters.loadParameters(self.basedir)

        self.csv_logger = CSVLogger(self.basedir, ServiceClassParameters.LOCAL_PARAMETERS["phase"])

        self.serviceReceiver = ServiceReceiver(basedir=self.basedir, csv_logger=self.csv_logger)

        self.recordSender = RecordSender(basedir=self.basedir)



    def start(self):
        """
        Start the Service Class Orchestrator.
        """

        print("Service Class started.")

        # Start the Service Receiver server
        self.serviceReceiver.start_server()


        if ServiceClassParameters.LOCAL_PARAMETERS["phase"] == "all_phases":
            print("All phases will be tested.")
            print("For each phase, the following sessions will be sent:")
            print(" Development: " + str(ServiceClassParameters.LOCAL_PARAMETERS["development_sessions"]))
            print(" Production: " + str(ServiceClassParameters.LOCAL_PARAMETERS["production_sessions"]))
            print(" Evaluation: " + str(ServiceClassParameters.LOCAL_PARAMETERS["evaluation_sessions"]))

            # Writing headers to the CSV file
            self.csv_logger.write_header("phase,timestamp,status")

            # Dictionary to store the phases and a boolean value to indicate if the labels should be sent
            phases_and_labels = {
                "development": True,
                "production": False,
                "evaluation": True
            }

            for phase, include_labels in phases_and_labels.items():

                if ServiceClassParameters.LOCAL_PARAMETERS[f"{phase}_sessions"] == 0:
                    print(f"Skipping {phase} phase.")
                    continue

                print("Starting " + phase + " phase.")

                # Preparing the bucket for the phase
                bucket = self.recordSender.prepare_bucket(ServiceClassParameters.LOCAL_PARAMETERS[f"{phase}_sessions"], include_labels)

                # Updating CSV file with the phase
                self.csv_logger.log(f"{phase},{time.time()},start")

                # Sending the bucket
                self.recordSender.send_bucket(bucket)

                # Updating CSV file
                self.csv_logger.log(f"{phase},{time.time()},records_sent")

                if phase == "development":
                    print("Waiting for the production configuration message.")

                    # Waiting for the production configuration message
                    configuration = self.serviceReceiver.get_configuration()

                    # Updating CSV file
                    self.csv_logger.log(f"{phase},{time.time()},{configuration['configuration']}")

                    if configuration["configuration"] != "production":
                        # Restart the development phase
                        print("Production configuration not received. Received " + configuration["configuration"] + " configuration.")
                        return

                else:
                    print(f"Waiting for {ServiceClassParameters.LOCAL_PARAMETERS[f'{phase}_sessions']} labels.")

                    # Waiting for the labels
                    for _ in range(ServiceClassParameters.LOCAL_PARAMETERS[f"{phase}_sessions"]):
                        label = self.serviceReceiver.get_label()

                    # Updating CSV file
                    self.csv_logger.log(f"{phase},{time.time()},labels_received")

                print(f"{phase.capitalize()} phase completed.")

            print("All phases completed.")

        elif ServiceClassParameters.LOCAL_PARAMETERS["phase"] == "development":
            print("Development phase will be tested, by developing " + str(ServiceClassParameters.LOCAL_PARAMETERS["classifiers_to_develop"]) + " classifiers.")

            # Writing headers to the CSV file
            self.csv_logger.write_header("developed_classifier,timestamp,status")

            for i in range(1, ServiceClassParameters.LOCAL_PARAMETERS["classifiers_to_develop"] + 1):
                print(f"Developing classifier {i}.")

                # Preparing the bucket for the development
                bucket = self.recordSender.prepare_bucket(ServiceClassParameters.LOCAL_PARAMETERS["development_sessions"], include_labels=True)

                # Updating CSV file with the classifier
                self.csv_logger.log(f"{i},{time.time()},start")

                # Sending the bucket
                self.recordSender.send_bucket(bucket)

                # Updating CSV file
                self.csv_logger.log(f"{i},{time.time()},records_sent")

        elif ServiceClassParameters.LOCAL_PARAMETERS["phase"] == "production":
            print("Production phase will be tested, by considering " + str(ServiceClassParameters.LOCAL_PARAMETERS["production_sessions"]) + " sessions.")

            # Writing headers to the CSV file
            self.csv_logger.write_header("sessions,timestamp,status")

            for i in range(1, ServiceClassParameters.LOCAL_PARAMETERS["production_sessions"]+1):

                print(f"Starting production phase {i}.")

                # Preparing the bucket for the production
                bucket = self.recordSender.prepare_bucket(i, include_labels=False)

                # Updating CSV file with the session
                self.csv_logger.log(f"{i},{time.time()},start")

                # Sending the bucket
                self.recordSender.send_bucket(bucket)

                # Updating CSV file
                self.csv_logger.log(f"{i},{time.time()},records_sent")

            print("All production phases completed.")

        else:
            print("Invalid value for the phase parameter.")
            print("Please, choose between 'all_phases', 'development' or 'production'.")
            return

        print("Service Class stopped.")


if __name__ == "__main__":

    service_class_orchestrator = ServiceClassOrchestrator()

    # Start the Service Class Orchestrator
    service_class_orchestrator.start()

    # Wait user input to stop the program
    input("Press Enter to stop the program...")
