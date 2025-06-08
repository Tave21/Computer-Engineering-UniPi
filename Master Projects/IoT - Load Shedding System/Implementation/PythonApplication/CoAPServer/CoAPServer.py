#!/usr/bin/env python

# Import CoAP base server class from CoAPthon
from coapthon.server.coap import CoAP
# Import custom CoAP resource handlers
from CoAPServer.resources.NodeRegistration import NodeRegistration
from CoAPServer.resources.CoAPDiscovery import CoAPDiscovery
from CoAPServer.resources.Clock import Clock
# Import custom logging utility
from Utility.Log import Log

# Initialize logger instance with module name and resource label
log_istance = Log(__name__, "EXP:Registration/Discovery")
# Get the logger object
log = log_istance.get_logger()

# Define the custom CoAPServer class inheriting from CoAP
class CoAPServer(CoAP):
    def __init__(self, host, port):
        # Initialize the CoAP server without multicast support
        CoAP.__init__(self, (host, port), multicast=False)
        # Register the NodeRegistration resource at /registration
        self.add_resource("/registration", NodeRegistration())
        # Register the CoAPDiscovery resource at /discovery
        self.add_resource("/discovery", CoAPDiscovery())
        # Register the Clock resource at /clock
        self.add_resource("/clock", Clock())
        # Log server startup message with IP and port
        log.info("CoAP Server started on {}:{}".format(host, port))