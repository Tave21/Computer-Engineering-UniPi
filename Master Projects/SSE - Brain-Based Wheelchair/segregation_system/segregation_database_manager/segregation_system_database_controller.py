import sqlite3
from typing import Any, Dict, List, Tuple
from segregation_system.prepared_session import PreparedSession


class SegregationSystemDatabaseController:
    """
    A unified class for managing SQLite databases and handling prepared sessions.

    This class provides methods to interact with a SQLite database, including
    creating tables, inserting records, updating, deleting, and fetching data.
    It also manages specific operations for the `prepared_session` table.
    """

    def __init__(self, db_name: str = "prepared_session_database"):
        """
        Initialize the unified database controller.

        Args:
            db_name (str): The name of the SQLite database file.
        """
        self.__database_name = db_name
        self.__table_name = "prepared_session"
        self.initialize_prepared_session_database()

    def _connect(self):
        """Establish a connection to the database."""
        return sqlite3.connect(self.__database_name)

    # Generic database methods
    def execute_query(self, query: str, params: Tuple = ()) -> None:
        """Execute a single query that does not return results."""
        try:
            with self._connect() as conn:
                cursor = conn.cursor()
                cursor.execute(query, params)
                conn.commit()
        except sqlite3.Error as e:
            print(f"Database error: {e}")

    def execute_script(self, script: str) -> None:
        """Execute a script containing multiple SQL statements."""
        try:
            with self._connect() as conn:
                cursor = conn.cursor()
                cursor.executescript(script)
                conn.commit()
        except sqlite3.Error as e:
            print(f"Database error: {e}")

    def fetch_query(self, query: str, params: Tuple = ()) -> List[Tuple]:
        """Execute a query and return the results."""
        try:
            with self._connect() as conn:
                cursor = conn.cursor()
                cursor.execute(query, params)
                return cursor.fetchall()
        except sqlite3.Error as e:
            print(f"Database error: {e}")
            return []

    def create_table(self, table_name: str, columns: Dict[str, str]) -> None:
        """Create a new table in the database."""
        columns_def = ", ".join([f"{col} {dtype}" for col, dtype in columns.items()])
        query = f"CREATE TABLE IF NOT EXISTS {table_name} ({columns_def})"
        self.execute_query(query)

    def insert(self, table_name: str, data: Dict[str, Any]) -> None:
        """Insert a record into a table."""
        columns = ", ".join(data.keys())
        placeholders = ", ".join(["?" for _ in data])
        query = f"INSERT INTO {table_name} ({columns}) VALUES ({placeholders})"
        self.execute_query(query, tuple(data.values()))

    def update(self, table_name: str, data: Dict[str, Any], condition: str, condition_params: Tuple) -> None:
        """Update records in a table based on a condition."""
        set_clause = ", ".join([f"{col} = ?" for col in data])
        query = f"UPDATE {table_name} SET {set_clause} WHERE {condition}"
        params = tuple(data.values()) + condition_params
        self.execute_query(query, params)

    def delete(self, table_name: str, condition: str, condition_params: Tuple) -> None:
        """Delete records from a table based on a condition."""
        query = f"DELETE FROM {table_name} WHERE {condition}"
        self.execute_query(query, condition_params)

    def fetch_all(self, table_name: str) -> List[Tuple]:
        """Fetch all records from a table."""
        query = f"SELECT * FROM {table_name}"
        return self.fetch_query(query)

    def fetch_where(self, table_name: str, condition: str, condition_params: Tuple) -> List[Tuple]:
        """Fetch records from a table based on a condition."""
        query = f"SELECT * FROM {table_name} WHERE {condition}"
        return self.fetch_query(query, condition_params)

    def drop_table(self, table_name: str) -> None:
        """Drop a table from the database."""
        query = f"DROP TABLE IF EXISTS {table_name}"
        self.execute_query(query)

    def drop_prepared_session_table(self) -> None:
        self.drop_table(self.__table_name)

    # Specific methods for prepared sessions
    def initialize_prepared_session_database(self):
        """Ensure the `prepared_session` table exists."""
        create_table_query = f"""
            CREATE TABLE IF NOT EXISTS {self.__table_name} (
                uuid TEXT PRIMARY KEY,
                label TEXT CHECK(label IN ('move', 'turnLeft', 'turnRight')) NOT NULL,
                psd_alpha_band REAL NOT NULL,
                psd_beta_band REAL NOT NULL,
                psd_theta_band REAL NOT NULL,
                psd_delta_band REAL NOT NULL,
                activity TEXT CHECK(activity IN ('shopping', 'sport', 'cooking', 'gaming', 'relax')) NOT NULL,
                environment TEXT CHECK(environment IN ('slippery', 'plain', 'slope', 'house', 'track')) NOT NULL
            );
        """
        self.execute_query(create_table_query)

    def store_prepared_session(self, data: Dict[str, Any]) -> None:
        """Store a prepared session into the `prepared_session` table."""
        self.insert(self.__table_name, data)

    def get_all_prepared_sessions(self) -> List[PreparedSession]:
        """Retrieve all prepared sessions as `PreparedSession` objects."""
        raw_prepared_sessions = self.fetch_all(self.__table_name)
        column_names = [
            "uuid", "label", "psd_alpha_band", "psd_beta_band",
            "psd_theta_band", "psd_delta_band", "activity", "environment"
        ]
        converted_sessions = [
            dict(zip(column_names, row)) for row in raw_prepared_sessions
        ]
        return [
            PreparedSession(
                uuid=session["uuid"],
                features=[
                    session["psd_alpha_band"], session["psd_beta_band"],
                    session["psd_theta_band"], session["psd_delta_band"],
                    session["activity"], session["environment"]
                ],
                label=session["label"]
            )
            for session in converted_sessions
        ]

    def get_number_of_prepared_session_stored(self) -> int:
        """Return the total number of prepared sessions."""
        query = f"SELECT COUNT(*) FROM {self.__table_name}"
        result = self.fetch_query(query)
        return result[0][0] if result else 0

    def remove_prepared_session(self, session_uuid: str) -> bool:
        """Remove a prepared session based on its UUID."""
        delete_query = f"DELETE FROM {self.__table_name} WHERE uuid = ?"
        self.execute_query(delete_query, (session_uuid,))
        return True

    def reset_session_database(self):
        """Reset the `prepared_session` database by dropping and recreating the table."""
        self.drop_prepared_session_table()
        self.initialize_prepared_session_database()
