import sqlite3
from typing import List, Tuple, Any, Dict


class DatabaseManager:
    """
    A general-purpose SQLite database manager class.

    Provides methods to interact with a SQLite database, including
    creating tables, inserting records, updating, deleting, and fetching data.
    """

    def __init__(self, db_name: str):
        """
        Initialize the DatabaseManager with a given database name.

        :param db_name: The name of the SQLite database file.
        """
        self.db_name = db_name

    def _connect(self):
        """Establish a connection to the database."""
        return sqlite3.connect(self.db_name)

    def execute_query(self, query: str, params: Tuple = ()) -> None:
        """
        Execute a single query that does not return results (INSERT, UPDATE, DELETE).

        :param query: The SQL query to execute.
        :param params: A tuple of parameters to bind to the query.
        """
        try:
            with self._connect() as conn:
                cursor = conn.cursor()
                cursor.execute(query, params)
                conn.commit()
        except sqlite3.Error as e:
            print(f"Database error: {e}")

    def execute_script(self, script: str) -> None:
        """
        Execute a script containing multiple SQL statements.

        :param script: The SQL script to execute.
        """
        try:
            with self._connect() as conn:
                cursor = conn.cursor()
                cursor.executescript(script)
                conn.commit()
        except sqlite3.Error as e:
            print(f"Database error: {e}")

    def fetch_query(self, query: str, params: Tuple = ()) -> List[Tuple]:
        """
        Execute a query and return the results (SELECT).

        :param query: The SQL query to execute.
        :param params: A tuple of parameters to bind to the query.
        :return: A list of tuples representing the fetched rows.
        """
        try:
            with self._connect() as conn:
                cursor = conn.cursor()
                cursor.execute(query, params)
                return cursor.fetchall()
        except sqlite3.Error as e:
            print(f"Database error: {e}")
            return []

    def create_table(self, table_name: str, columns: Dict[str, str]) -> None:
        """
        Create a new table in the database.

        :param table_name: The name of the table.
        :param columns: A dictionary of column names and their SQLite data types.
        """
        columns_def = ", ".join([f"{col} {dtype}" for col, dtype in columns.items()])
        query = f"CREATE TABLE IF NOT EXISTS {table_name} ({columns_def})"
        self.execute_query(query)

    def insert(self, table_name: str, data: Dict[str, Any]) -> None:
        """
        Insert a record into a table.

        :param table_name: The name of the table.
        :param data: A dictionary of column names and values to insert.
        """
        columns = ", ".join(data.keys())
        placeholders = ", ".join(["?" for _ in data])
        query = f"INSERT INTO {table_name} ({columns}) VALUES ({placeholders})"
        self.execute_query(query, tuple(data.values()))

    def update(self, table_name: str, data: Dict[str, Any], condition: str, condition_params: Tuple) -> None:
        """
        Update records in a table based on a condition.

        :param table_name: The name of the table.
        :param data: A dictionary of columns and their new values.
        :param condition: The WHERE clause to select the records to update.
        :param condition_params: A tuple of parameters to bind to the WHERE clause.
        """
        set_clause = ", ".join([f"{col} = ?" for col in data])
        query = f"UPDATE {table_name} SET {set_clause} WHERE {condition}"
        params = tuple(data.values()) + condition_params
        self.execute_query(query, params)

    def delete(self, table_name: str, condition: str, condition_params: Tuple) -> None:
        """
        Delete records from a table based on a condition.

        :param table_name: The name of the table.
        :param condition: The WHERE clause to select the records to delete.
        :param condition_params: A tuple of parameters to bind to the WHERE clause.
        """
        query = f"DELETE FROM {table_name} WHERE {condition}"
        self.execute_query(query, condition_params)

    def fetch_all(self, table_name: str) -> List[Tuple]:
        """
        Fetch all records from a table.

        :param table_name: The name of the table.
        :return: A list of tuples representing the fetched rows.
        """
        query = f"SELECT * FROM {table_name}"
        return self.fetch_query(query)

    def fetch_where(self, table_name: str, condition: str, condition_params: Tuple) -> List[Tuple]:
        """
        Fetch records from a table based on a condition.

        :param table_name: The name of the table.
        :param condition: The WHERE clause to select the records.
        :param condition_params: A tuple of parameters to bind to the WHERE clause.
        :return: A list of tuples representing the fetched rows.
        """
        query = f"SELECT * FROM {table_name} WHERE {condition}"
        return self.fetch_query(query, condition_params)

    def drop_table(self, table_name: str) -> None:
        """
        Drop a table from the database.

        :param table_name: The name of the table to drop.
        """
        query = f"DROP TABLE IF EXISTS {table_name}"
        self.execute_query(query)


# To Test the DatabaseManager class
if __name__ == "__main__":
    # Initialize the database manager
    db = DatabaseManager('example.db')

    # Create a table
    db.create_table('users', {
        'id': 'INTEGER PRIMARY KEY AUTOINCREMENT',
        'name': 'TEXT NOT NULL',
        'age': 'INTEGER',
        'email': 'TEXT'
    })

    # Insert a record
    db.insert('users', {'name': 'John Doe', 'age': 30, 'email': 'john@example.com'})

    # Fetch records
    users = db.fetch_all('users')
    print(users)

    # Update a record
    db.update('users', {'age': 31}, 'name = ?', ('John Doe',))

    # Delete a record
    db.delete('users', 'name = ?', ('John Doe',))
