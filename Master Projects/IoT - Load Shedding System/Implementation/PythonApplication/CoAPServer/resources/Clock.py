# Import base CoAP Resource class
from coapthon.resources.resource import Resource
# Import custom logging utility
from Utility.Log import Log
# Import Python datetime module
from datetime import datetime

# Define a CoAP resource named "Clock" that provides the current server time
class Clock(Resource):

    # Constructor initializes the resource with a default payload of the current time (non-observable)
    def __init__(self, name="CoapClock"):
        super(Clock, self).__init__(name, observable=False)
        # Set the initial payload to the current time in ISO 8601 format
        self.payload = datetime.now().strftime("%Y-%m-%dT%H:%MZ")

    # Handle incoming GET requests to this resource
    def render_GET(self, request):
        # Create a new response instance of the Clock resource
        res = Clock()
        res.location_query = request.uri_query

        # Initialize the logger with context "Clock"
        log = Log("Clock", request.payload).get_logger()

        # Update the payload to the current time
        res.payload = datetime.now().strftime("%Y-%m-%dT%H:%MZ")

        # Log the request and the time being sent back
        log.info("Request time by {} sending: {}".format(request.source[0], res.payload))

        # Return the response object with the updated time
        return res
