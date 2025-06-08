# Import the logging module for logging capabilities
import logging

# Define a custom logging Filter class to attach a 'resource' attribute to log records
class Resource(logging.Filter):
    def __init__(self, resource):
        # Call the base class constructor
        super(Resource, self).__init__()
        # Store the resource name
        self.resource = resource
    
    # Filter method that adds the resource attribute to each log record
    def filter(self, record):
        record.resource = self.resource
        return True
    
    # Method to update the resource value dynamically
    def update_resource(self, resource):
        self.resource = resource

# Define the main logging utility class
class Log:
    def __init__(self, module, resource):
        # Create a filter instance with the provided resource name
        self.filter = Resource(resource)

        # Create or get a logger with the specified module name
        self.logger = logging.getLogger(module)

        # Set the logging level to INFO by default
        self.logger.setLevel(logging.INFO)

        # Create a stream handler to output logs to the console
        self.handler = logging.StreamHandler()

        # Define a formatter with timestamp, log level, module, resource, and message
        self.formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(module)s - %(resource)s - %(message)s', datefmt='%Y-%m-%d %H:%M:%S')

        # Set the formatter for the handler
        self.handler.setFormatter(self.formatter)

        # Add the handler to the logger
        self.logger.addHandler(self.handler)

        # Add the custom resource filter to the logger
        self.logger.addFilter(self.filter)
    
    # Method to change the logging level dynamically
    def set_level(self, level):
        self.logger.setLevel(level)
    
    # Method to change the resource label dynamically
    def set_resource(self, resource):
        self.filter.update_resource(resource)
    
    # Method to retrieve the configured logger
    def get_logger(self):
        return self.logger
