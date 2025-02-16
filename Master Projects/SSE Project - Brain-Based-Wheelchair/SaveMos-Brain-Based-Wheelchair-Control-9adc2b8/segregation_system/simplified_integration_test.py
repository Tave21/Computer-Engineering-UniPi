from time import sleep

from segregation_system.SegregationSystemJsonHandler import SegregationSystemJsonHandler
from segregation_system.learning_set_splitter import LearningSetSplitter
from segregation_system.prepared_session import PreparedSession
from segregation_system.segregation_database_manager.segregation_system_database_controller import \
    SegregationSystemDatabaseController
from segregation_system.segregation_system_parameters import SegregationSystemConfiguration
from segregation_system.session_receiver_and_configuration_sender import SessionReceiverAndConfigurationSender

if __name__ == "__main__":
    SegregationSystemConfiguration.configure_parameters()
    message_broker = SessionReceiverAndConfigurationSender()
    message_broker.start_server()

    json_handler = SegregationSystemJsonHandler()
    #message_broker.send_timestamp(time.time(), "start segregation")

    db = SegregationSystemDatabaseController()
    db.reset_session_database()

    while db.get_number_of_prepared_session_stored() < 10:
        print("wait for message...")
        message = message_broker.get_last_message()
        print("Message received ", message)
        # Receive the prepared session from the preparation system, and cast it into a PreparedSession object.
        new_prepared_session = PreparedSession.from_dictionary(json_handler.string_to_dict(message['message']))

        # Store the new prepared session in the database.
        db.store_prepared_session(new_prepared_session.to_dictionary())

    Sessions = db.get_all_prepared_sessions()

    LearningSetSplitter = LearningSetSplitter()
    Set = LearningSetSplitter.generateLearningSets(Sessions)

    network_info = SegregationSystemConfiguration.GLOBAL_PARAMETERS["Development System"]

    print("sending the message...")

    message_broker.send_message(network_info['ip'], network_info['port'], SegregationSystemJsonHandler.dict_to_string(Set.to_dict()))

    print("message sent successfully")

    sleep(30)
