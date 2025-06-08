# Import the HelperClient class to communicate with CoAP servers
from coapthon.client.helperclient import HelperClient
# Import the custom DB access class
from Utility.DBAccess import DBAccess
# Import the ConfigParser to read configuration files
from configparser import ConfigParser
# Import datetime to work with timestamps
from datetime import datetime
# Import CoAP protocol definitions
import coapthon.defines as defines
# Import JSON for parsing data
import json
# Import regular expressions (unused in current code)
import re
# Import custom logging utility
from Utility.Log import Log

# Initialize a logger instance with "Unknown" as resource
log_istance = Log(__name__, "Unknown")
log = log_istance.get_logger()

# Class to parse and manage SenML-formatted JSON data
class jsonSenML:
    def __init__(self):
        # Decimal accuracy used to convert integer data to float
        self._decimal_accuracy = 10000
        # Expected timestamp format
        self._date_format = "%Y-%m-%dT%H:%MZ"
        # Variable to store the parsed JSON
        self.json = None
        
    # Initialize the object by decoding a JSON string
    def initialize(self, json_str):
        try:
            self.json = json.loads(json_str)
        except json.JSONDecodeError as e:
            log.error("Failed to decode JSON: {}".format(e.msg))
            log.error("Bad JSON: {}".format(json_str))
            return True
        except Exception as e:
            log.error("BAD JSON:", e)
            return True
        
        # Normalize values if they exceed accuracy threshold or are negative
        for i in range(1, len(self.json)):
            if self.json[i]['v'] >= self._decimal_accuracy:
                self.json[i]['v'] = self.json[i]['v'] / self._decimal_accuracy
            if self.json[i]['v'] < 0:
                log.info("Data received is < 0, skipping...")
                return True
        return False
    
    # Return datetime object parsed from JSON timestamp
    def get_datetime(self, index):
        if self.json is None:
            log.error("JSON is None")
            return None
        try:
            return datetime.strptime(self.json[index]['t'], self._date_format)
        except ValueError as e:
            log.error("Failed to parse the date: {}".format(e))
            return None

# Observer class to monitor CoAP resources and update database
class CoAPObserver:
    def __init__(self, ip, resource):
        # Initialize the CoAP client with the IP and port
        self.client = HelperClient(server=(ip, 5683))
        # Store the resource path to observe
        self.resource = resource
        # Store the last received response
        self.last_response = None

    # Check if the record is already in the database, and update or insert accordingly
    def check_and_update(self, type, data, db):
        # SQL query to check if data for the same hour exists
        search_query = "SELECT COUNT(*) FROM `load` WHERE YEAR(timestamp) = %s AND MONTH(timestamp) = %s AND DAY(timestamp) = %s AND HOUR(timestamp) = %s AND MINUTE(timestamp) = %s"
        # SQL query to insert a new record
        insert_query = "INSERT INTO `load`(timestamp,{}) VALUES (%s,%s)".format(type)
        # SQL query to update an existing record
        update_query = "UPDATE `load` SET {} = %s WHERE YEAR(timestamp) = %s AND MONTH(timestamp) = %s AND DAY(timestamp) = %s AND HOUR(timestamp) = %s AND MINUTE(timestamp) = %s".format(type)
        
        # Determine which index in the JSON array to read
        index = 2
        if type == "predicted":
            index = 1    

        # Get the datetime from the JSON data
        time = data.get_datetime(index)
        if time is None:
            log.error("Failed get the timestamp from the data")
            return True

        # Format the datetime for the SQL query
        db_timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
        val = (time.year, time.month, time.day, time.hour, time.minute)
        result = db.query(search_query, val, True)
        if result is None:
            log.error("Failed to find the data in the database")
            return True
        
        # If data exists, update it
        if result[0][0] != 0:
            val = (data.json[index]["v"], time.year, time.month, time.day, time.hour, time.minute)
            ret = db.query(update_query, val, False)
        else:
            # Otherwise insert new data
            val = (db_timestamp, data.json[index]["v"])
            ret = db.query(insert_query, val, False)
        return False

    # Callback function for CoAP observe responses
    def callback_observe(self, response):
        # Set the resource context in the logger
        log_istance.set_resource(self.resource)
        log.info("Callback Observe {}".format(self.resource))
        
        # Validate response content
        if response is None or response.code != defines.Codes.CONTENT.number:
            log.error("Response is None or not content")
            return None

        # Read configuration for database access
        configur = ConfigParser()
        configur.read('./CoAPServer/config.ini')
        database = DBAccess(
            host = configur.get('mysql', 'host'),
            user = configur.get('mysql', 'user'),
            password = configur.get('mysql', 'password'),
            database = configur.get('mysql', 'database'),
            log=log)
        if database.connect() is None:
            return None
        
        # Parse the JSON response into a SenML object
        data = jsonSenML()
        if data.initialize(response.payload) is True:
            database.close()
            return None
        
        # If the resource is 'load', process both sampled and predicted data
        if self.resource == 'load':
            check = self.check_and_update("sampled", data, database)
            if check is True:
                database.close()
                return None
            self.check_and_update("predicted", data, database)
            log.info("Updated load")
        elif self.resource == 'status':
            self.query = "INSERT INTO relay(timestamp, status) VALUES (%s, %s)"
            time = data.get_datetime(1)
            db_timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
            val = (db_timestamp, data.json[1]["v"])
            ret = database.query(self.query, val, False)

            if ret is None:
                log.error("Failed to insert status information into the database: {}".format(val))
                return None
            else:
                log.info("Updated status")

            ts = db_timestamp  

            rows = database.query(
                "SELECT id, status FROM relay WHERE timestamp = %s ORDER BY id DESC",
                (ts,),
                fetchall=True
            )

            if rows and len(rows) == 2:
                (id1, s1), (id2, s2) = rows

                swap_sql = """
                  UPDATE relay
                     SET status = CASE id
                                    WHEN %s THEN %s
                                    WHEN %s THEN %s
                                  END
                   WHERE id IN (%s, %s)
                """
                params = (id1, s2, id2, s1, id1, id2)
                database.query(swap_sql, params, fetchall=False)
        # Store the latest response
        self.last_response = response

    # Start observing the resource
    def start(self):
        self.client.observe(self.resource, self.callback_observe)
    
    # Stop observing the resource
    def stop(self):
        self.client.cancel_observing(self.last_response)
        self.client.stop()
