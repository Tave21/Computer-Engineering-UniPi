# Import the MySQL connector module to interact with a MySQL database
import mysql.connector

# Define a class to manage access to the database
class DBAccess():
    def __init__(self, host, user, password, database, log):
        # Store database connection parameters
        self.host = host
        self.user = user
        self.password = password
        self.database = database
        # Store the logger instance
        self.log = log

        # Initialize connection and cursor attributes to None
        self.mydb = None
        self.mycursor = None

    # Method to connect to the MySQL database
    def connect(self):
        try:
            # Connect to the MySQL database using provided credentials
            self.mydb = mysql.connector.connect(
                host=self.host,
                user=self.user,
                password=self.password,
                database=self.database
            )
            # Create a cursor object to execute SQL queries
            self.mycursor = self.mydb.cursor()
        except Exception as e:
            # Log an error message if connection fails
            self.log.error("Could not connect to the database: {}".format(e))
            return None
        return True
    
    # Method to close the database connection
    def close(self):
        self.mydb.close()
        self.mydb = None
        self.mycursor = None
    
    # Method to execute a query on the database
    def query(self, query, val, fetchall=False):
        """Query the database with the given query and values, return the result of the query if it is a SELECT query, otherwise return True if the query was successful."""

        # Check if the database connection is established
        if self.mydb is None:
            # Log an error if not connected
            self.log.error("Database connection is not established")
            return None

        try:
            # Execute the SQL query with the given values
            self.mycursor.execute(query, val)
        except Exception as e:
            # Log an error if the query execution fails
            self.log.error("Could not execute the query: {}".format(e))
            return None

        # Check if the caller expects results from a SELECT query
        if fetchall:
            # Fetch and return all results
            result = self.mycursor.fetchall()
        else:
            # Commit the changes for INSERT, UPDATE or DELETE queries
            self.mydb.commit()
            result = True

        return result
