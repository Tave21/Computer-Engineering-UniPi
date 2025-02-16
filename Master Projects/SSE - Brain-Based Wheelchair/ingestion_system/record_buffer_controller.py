"""
Module: record_buffer_controller
This module handles the buffering of records in the ingestion system.

Author: Francesco Taverna


"""
import json

from .Ingestion_Database_Manager.database_manager import DatabaseManager


class RecordBufferController:
    """
    Controller for managing the record buffer.
    Provides methods to store, retrieve, and remove records from the buffer.
    """

    def __init__(self):
        """
        Initialize the buffer.
        """
        """
        Structure of data:
         - Application => raw inputs
            (1) *helmet => UUID, EEG-data (time series in a fixed period of time of each feature );
            (2) calendar => UUID, activity(shopping, sport, cooking, relax, gaming);
            (3) environment => UUID, environment(slippery, plain, slope, house, track);
         - Output
            (4) labels => UUID, movements(move, turn left, turn right).
         - Features (net input) 
            (a) PSD alpha band
            (b) PSD beta band
            (c) PSD tetha band
            (d) PSD delta band
            (e) activity + small scatter
            (f) environment + small scatter
        """

        #create Database class instance
        self.db = DatabaseManager()
        self.db.drop_database()
        #prepare query to create records table if it doesn't exist
        query = ("CREATE TABLE IF NOT EXISTS records \
                    (uuid TEXT PRIMARY KEY, \
                     environment TEXT, \
                     labels TEXT, \
                     helmet TEXT, \
                     calendar TEXT);")
        #perform query
        self.db.create_table(query)

    def store_record(self, record):
        """
              stores a record in the IngestionSystem.db.
              :param record: record to store
        """

        # each record has uuid
        uuid = record["value"]["UUID"]
        #record example:
        """
         {
            "value": {
                "UUID": "a923-45b7-gh12-166"
                "environment": "slippery"
            }
            "source": "environment"
         }
        """
        # adds a new row in records table if uuid PK does not exist
        query = ("INSERT OR IGNORE INTO records (uuid, environment, labels, helmet, calendar) "
                "VALUES (?, NULL, NULL, NULL, NULL);")
        values = (uuid,)
        self.db.perform_query('insert', query, values)

        # gets values for query --> get a dictionary without UUID (just the value of the column indicated in "source")
        record_filtered = {key: value for key, value in record["value"].items() if key != "UUID"}
        #convert python dictionary to json
        json_value = json.dumps(record_filtered)

        #get column name from record
        column_name = record["source"]
        #prepare update query with the new value of the column received in the record
        query = f"UPDATE records SET {column_name} = ? WHERE uuid = ?;"
        values = (json_value, uuid)

        #execute update
        self.db.perform_query('update', query, values)
        return

    def get_records(self, uuid: str) :
        """
              retrieves a record with an uuid from the db.
              :param uuid: uuid of record
              :return: Record
        """
        #prepare query
        query = ("SELECT * "
                 "FROM records "
                 "WHERE uuid = ? ;")
        values = (uuid,)
        rows = self.db.perform_query('select', query, values)

        # converts result to list
        row = list(rows[0]) #there will be only one row, thus take the first element

        # converts string to json
        #uuid is the same, then for each value of the first, second .. columns if it is not null
        #it is read as json and converted in python dictionary, then converted in list
        result = [row[0]] + [value if value is None else list(json.loads(value).values()) for value in row[1:]]

        #if there is a list of 1 element as element of the list, it is extracted
        result = [item[0] if isinstance(item, list) and len(item) == 1 else item for item in result]

        return result #row made of 3/4 records + uuid


    def remove_records(self, uuid: str) -> None:
         """
         deletes a record from the db.
         :param uuid: uuid of record
         """

         query = ("DELETE FROM records "
                  "WHERE uuid = ?;")
         values = (uuid,)
         #perform delete query
         self.db.perform_query('delete', query, values)
         return
