# Import the CoAPServer class from the CoAPServer module
from CoAPServer.CoAPServer import CoAPServer

# Define the IPv6 address for the server
ip = "fd00::1"

# Define the port number for the server
port = 5683

# Create an instance of the CoAPServer with the given IP and port
server = CoAPServer(ip, port)

try:
    # Start listening for incoming CoAP requests (timeout after 10 seconds)
    server.listen(10)
except KeyboardInterrupt:
    # Handle interruption (e.g., Ctrl+C) and shut down the server gracefully
    print("Server Shutdown")
    server.close()
    print("Exiting...")
