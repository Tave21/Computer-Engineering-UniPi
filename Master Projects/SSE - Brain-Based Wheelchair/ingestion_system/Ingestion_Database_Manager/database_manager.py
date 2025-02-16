"""
Author: Francesco Taverna
"""
import contextlib
import os
import sqlite3

from ingestion_system import DATABASE_FOLDER_PATH, DATABASE_FILE_PATH


class DatabaseManager:
    """
    Class responsible for handling low-level database operations.
    """

    def __init__(self):

        # create report folder
        try:
            os.mkdir(DATABASE_FOLDER_PATH)
        except FileExistsError:
            pass

        # flush it if already exists
        except FileExistsError:
            for file in os.listdir(DATABASE_FOLDER_PATH):
                os.remove(DATABASE_FOLDER_PATH + file)

        #create database
        self.__create_database()

    def __create_database(self): #private function
        """
        Create an SQLite database if it does not already exist.

        """

        if not os.path.exists(DATABASE_FILE_PATH):
            #create IngestionSystem.db file
            conn = sqlite3.connect(DATABASE_FILE_PATH)
            conn.close()

    def __execute_commit_query(self, query: str, values: tuple = None):
        """
        Execute a SQL query with optional parameter substitution and commit the transaction.

        :param query: SQL query to execute.
        :param values: Tuple of values to substitute in the query (optional).
        """

        # Connect to the database
        #after with block, the connection is closed
        with contextlib.closing(sqlite3.connect(DATABASE_FILE_PATH)) as conn:
            # after with block, the cursor is closed
            with contextlib.closing(conn.cursor()) as cursor:
                # Execute the query with optional parameter substitution
                if values is not None:
                    #values will substitute ? query placeholder
                    cursor.execute(query, values)
                else:
                    cursor.execute(query)
                # Commit the transaction
                conn.commit()

    def perform_query(self, operation_type, query: str, values: tuple = None):
        """
        Perform a database query based on the operation type.

        :param operation_type: Type of the operation (e.g., 'insert', 'delete', 'update', 'select').
        :param query: SQL query to execute.
        :param values: Tuple of values to substitute in the query (optional).
        """
        if operation_type not in ['insert', 'delete', 'update', 'select']:
            raise ValueError("Invalid operation type. Supported types: 'insert', 'delete', 'update', 'select'.")

        if values is not None and not isinstance(values, tuple):
            raise ValueError("Values must be a tuple or None.")

        if operation_type == 'select':
            with contextlib.closing(sqlite3.connect(DATABASE_FILE_PATH)) as conn:
                with contextlib.closing(conn.cursor()) as cursor:
                    if values is not None:
                        cursor.execute(query, values)
                    else:
                        cursor.execute(query)
                    return cursor.fetchall() #tuple list returned
        else:
            self.__execute_commit_query(query, values)
            return True #query successfull

    def create_table(self, query: str):
        """
        Create a table in the database.

        :param query: SQL query for creating the table.
        """
        self.__execute_commit_query(query)

    def delete_table(self, table: str):
        """
        Delete a table from the database.

        :param table: Name of the table to delete.
        """
        self.__execute_commit_query(f"DROP TABLE IF EXISTS {table};")

    def drop_database(self):
        """
        Drop the entire database.
        """
        if os.path.exists(DATABASE_FILE_PATH):
            os.remove(DATABASE_FILE_PATH)

    def get_database_path(self):
        """
        Retrieve the path of the SQLite database file.

        :return: Path of the SQLite database file.
        """
        return DATABASE_FILE_PATH
