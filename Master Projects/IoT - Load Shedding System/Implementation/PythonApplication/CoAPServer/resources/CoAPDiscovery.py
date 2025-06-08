# Import the CoAP resource base class
from coapthon.resources.resource import Resource
# Import a helper client for CoAP requests
from coapthon.client.helperclient import HelperClient
# Import CoAP protocol constants
from coapthon import defines
import coapthon.defines as defines
# Import configuration parser
from configparser import ConfigParser
# Import custom database access class
from Utility.DBAccess import DBAccess
# Import custom logging utility
from Utility.Log import Log

# Class representing a CoAP resource for node discovery
class CoAPDiscovery(Resource):

    # Constructor initializes the resource as not observable
    def __init__(self, name="CoAP Discovery"):
        super(CoAPDiscovery, self).__init__(name, observable=False)
        self.payload = ""

    # Method to check whether a given CoAP resource is reachable on a node
    def check_resource(self, host, port, resource):
        """Check if the resource is available in the node and if the node is active"""
        client = HelperClient(server=(host, port))
        response = client.get(resource)
        client.stop()
        # If there is no valid response, the resource is unavailable
        if response is None or response.code != defines.Codes.CONTENT.number:
            return False
        else:
            return True

    # Method to handle GET requests to this resource
    def render_GET(self, request):
        # Create a new instance of CoAPDiscovery to return as response
        res = CoAPDiscovery()
        res.location_query = request.uri_query

        # Create logger for this request using the payload as context
        log = Log("CoAPObserver", request.payload).get_logger()

        # Log the requested resource
        log.info("Requested ip resource: {}".format(request.payload))

        # Initialize database connection using configuration file
        configur = ConfigParser()
        configur.read('./CoAPServer/config.ini')
        database = DBAccess(
            host = configur.get('mysql', 'host'),
            user = configur.get('mysql', 'user'),
            password = configur.get('mysql', 'password'),
            database = configur.get('mysql', 'database'),
            log=log)

        # Connect to the database
        if database.connect() is None:
            return None

        # Query to check whether the requested resource exists in the database
        query = "SELECT COUNT(*) FROM nodes WHERE resource = %s"
        val = (request.payload,)
        node_ip = database.query(query, val, True)

        # If query failed or resource does not exist
        if node_ip is None:
            database.close()
            return None
        elif node_ip[0][0] <= 0:
            log.error("Resource not found:{}".format(node_ip[0][0]))
            return None

        # Retrieve the IP address of the node hosting the resource
        query = "SELECT ip FROM nodes WHERE resource = %s"
        node_ip = database.query(query, val, True)
        if node_ip is None:
            database.close()
            return None

        # Close the database connection after verification
        database.close()

        # Log and return the IP address of the node with the resource
        log.info("Resource found at: {}, sending to request".format(node_ip[0][0]))
        res.payload = node_ip[0][0]

        return res
