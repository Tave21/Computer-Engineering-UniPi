# Import CoAP resource base class
from coapthon.resources.resource import Resource
# Import JSON parser
import json
# Import JSON schema validator
from jsonschema import validate, ValidationError
# Import custom database access module
from Utility.DBAccess import DBAccess
# Import CoAP protocol definitions
import coapthon.defines as defines
# Import configuration file reader
from configparser import ConfigParser
# Import custom CoAP observer class
from CoAPServer.CoAPObserver import CoAPObserver
# Import CoAP response message class
from coapthon.messages.response import Response
# Import custom logging utility
from Utility.Log import Log

# Initialize a logger with the context "Unknown"
log_istance = Log(__name__, "Unknown")
log = log_istance.get_logger()

# Class for handling CoAP POST registration requests
class NodeRegistration(Resource):

    # Constructor initializes the resource as not observable
    def __init__(self, name="Node Registration"):
        super(NodeRegistration, self).__init__(name, observable=False)
        # Default payload to respond with
        self.payload = "Registered"
        
    # Handles incoming POST requests for node registration
    def render_POST(self, request):
        # Create a new instance of NodeRegistration to return as response
        res = NodeRegistration()
        
        # Define expected JSON schema for registration payload
        json_register_schema = {
            "node": {"type": "string"},
            "resource": {"type": "string"},
            "settings": {"type": "string"}
        }

        # Store URI query parameters, if any
        res.location_query = request.uri_query
        log.info("Node Registration POST request received from: {}".format(request.source[0]))

        # Try parsing and validating the incoming JSON payload
        try:
            node_info = json.loads(request.payload)
            validate(instance=node_info, schema=json_register_schema)
        except ValidationError as e:
            log.error("JSON is not valid: ", e.message)
            log.error("\t - BAD JSON:", node_info)
            return None
        except json.JSONDecodeError as e:
            log.error("Failed to decode JSON: {}".format(e.msg))
            log.error("Bad JSON: {}".format(request.payload))
            return None
        except Exception as e:
            log.error("BAD PAYLOAD:", e)
            return None

        # Update logger context with node name
        log_istance.set_resource(node_info["node"])
        log.info("Node Registration POST source recognized")

        # Prepare SQL query to insert or update node registration info
        query = "REPLACE INTO nodes (ip, name ,resource, settings) VALUES (%s, %s, %s, %s)"
        val = (request.source[0], node_info["node"], str(node_info["resource"]), str(node_info["settings"]))

        # Read database configuration
        configur = ConfigParser()
        configur.read('./CoAPServer/config.ini')
        # Initialize database access object
        database = DBAccess(
            host = configur.get('mysql', 'host'),
            user = configur.get('mysql', 'user'),
            password = configur.get('mysql', 'password'),
            database = configur.get('mysql', 'database'),
            log=log)
        # Connect to the database
        if database.connect() is None:
            return None

        # Execute the query
        ret = database.query(query, val, False)
        # Close the database connection
        database.close()

        # If the insert/update failed, return None
        if ret is None:
            log.error("Failed to insert node information into the database")
            return None
        
        log.info("Inserted node information into the database")

        # Start observing the specified resource on the registering node
        log.info("Starting observation of the resource: {} from {}".format(node_info["resource"], request.source[0]))
        observer = CoAPObserver(request.source[0], node_info["resource"])
        observer.start()

        # Reset logger resource context
        log_istance.set_resource("Unknown")

        # Return the registration confirmation resource
        return res