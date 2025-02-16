"""
Author: Giovanni Ligato
"""

import sqlite3
from typing import List, Tuple
from evaluation_system.Label import Label

class LabelsBuffer:
    """
    A specialized SQLite database manager class for storing Label instances.
    """

    def __init__(self, db_name: str = "labels.db"):
        """
        Initialize the LabelsBuffer with a given database name.

        :param db_name: The name of the SQLite database file.
        """
        self.db_name = db_name
        self.create_table()

    def _connect(self):
        """Establish a connection to the database."""
        return sqlite3.connect(self.db_name)

    def create_table(self) -> None:
        """
        Create the labels table in the database.
        """

        query = """
        CREATE TABLE IF NOT EXISTS labels (
            uuid TEXT NOT NULL,
            movements INTEGER NOT NULL,
            expert INTEGER NOT NULL,
            PRIMARY KEY (uuid, expert)
        )
        """

        self.execute_query(query)

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

    def save_label(self, label: Label) -> None:
        """
        Save a Label instance to the database.

        :param label: The Label instance to save.
        """
        query = """
        INSERT OR REPLACE INTO labels (uuid, movements, expert)
        VALUES (?, ?, ?)
        """
        self.execute_query(query, (label.uuid, label.movements, label.expert))

    def get_classifier_labels(self, limit: int) -> List[Label]:
        """
        Get the first limit classifier labels from the database.

        :param limit: The maximum number of labels to fetch.
        :return: A list of Label instances.
        """
        query = "SELECT uuid, movements, expert FROM labels WHERE expert = 0 ORDER BY uuid LIMIT ?"
        rows = self.fetch_query(query, (limit,))
        return [Label(uuid=row[0], movements=row[1], expert=row[2]) for row in rows]

    def get_expert_labels(self, limit: int) -> List[Label]:
        """
        Get the first limit expert labels from the database.

        :param limit: The maximum number of labels to fetch.
        :return: A list of Label instances.
        """
        query = "SELECT uuid, movements, expert FROM labels WHERE expert = 1 ORDER BY uuid LIMIT ?"
        rows = self.fetch_query(query, (limit,))
        return [Label(uuid=row[0], movements=row[1], expert=row[2]) for row in rows]

    def delete_labels(self, limit: int) -> None:
        """
        Delete the first limit labels from the database.
        Both limit classifier labels and limit expert labels will be deleted.

        :param limit: The maximum number of labels to delete.
        """
        # Delete the first limit classifier labels
        query_classifier = """
        DELETE FROM labels WHERE rowid IN (
            SELECT rowid FROM labels WHERE expert = 0 ORDER BY uuid LIMIT ?
        )
        """
        self.execute_query(query_classifier, (limit,))

        # Delete the first limit expert labels
        query_expert = """
        DELETE FROM labels WHERE rowid IN (
            SELECT rowid FROM labels WHERE expert = 1 ORDER BY uuid LIMIT ?
        )
        """
        self.execute_query(query_expert, (limit,))

    def get_num_classifier_labels(self) -> int:
        """
        Get the number of classifier labels in the database.

        :return: The number of classifier labels.
        """
        query = "SELECT COUNT(*) FROM labels WHERE expert = 0"
        result = self.fetch_query(query)
        return result[0][0] if result else 0

    def get_num_expert_labels(self) -> int:
        """
        Get the number of expert labels in the database.

        :return: The number of expert labels.
        """

        query = "SELECT COUNT(*) FROM labels WHERE expert = 1"
        result = self.fetch_query(query)
        return result[0][0] if result else 0

