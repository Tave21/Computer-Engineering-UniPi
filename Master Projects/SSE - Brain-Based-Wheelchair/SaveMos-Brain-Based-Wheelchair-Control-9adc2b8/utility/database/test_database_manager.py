import unittest

from database_manager import DatabaseManager


class TestDatabaseManager(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        cls.db_name = "test_database.db"
        cls.db_manager = DatabaseManager(cls.db_name)

    def setUp(self):
        self.db_manager.create_table("test_table", {
            "id": "INTEGER PRIMARY KEY AUTOINCREMENT",
            "name": "TEXT",
            "age": "INTEGER"
        })

    def tearDown(self):
        self.db_manager.drop_table("test_table")

    def test_execute_query(self):
        self.db_manager.execute_query("INSERT INTO test_table (name, age) VALUES (?, ?)", ("Alice", 30))
        results = self.db_manager.fetch_all("test_table")
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0][1], "Alice")

    def test_execute_script(self):
        script = """
        INSERT INTO test_table (name, age) VALUES ('Bob', 25);
        INSERT INTO test_table (name, age) VALUES ('Charlie', 35);
        """
        self.db_manager.execute_script(script)
        results = self.db_manager.fetch_all("test_table")
        self.assertEqual(len(results), 2)

    def test_fetch_query(self):
        self.db_manager.execute_query("INSERT INTO test_table (name, age) VALUES (?, ?)", ("Alice", 30))
        results = self.db_manager.fetch_query("SELECT * FROM test_table WHERE name = ?", ("Alice",))
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0][1], "Alice")

    def test_create_table(self):
        self.db_manager.create_table("new_table", {"col1": "TEXT", "col2": "INTEGER"})
        tables = self.db_manager.fetch_query("SELECT name FROM sqlite_master WHERE type='table' AND name='new_table'")
        self.assertEqual(len(tables), 1)
        self.db_manager.drop_table("new_table")

    def test_insert(self):
        self.db_manager.insert("test_table", {"name": "Alice", "age": 30})
        results = self.db_manager.fetch_all("test_table")
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0][1], "Alice")

    def test_update(self):
        self.db_manager.insert("test_table", {"name": "Alice", "age": 30})
        self.db_manager.update("test_table", {"age": 35}, "name = ?", ("Alice",))
        results = self.db_manager.fetch_all("test_table")
        self.assertEqual(results[0][2], 35)

    def test_delete(self):
        self.db_manager.insert("test_table", {"name": "Alice", "age": 30})
        self.db_manager.delete("test_table", "name = ?", ("Alice",))
        results = self.db_manager.fetch_all("test_table")
        self.assertEqual(len(results), 0)

    def test_fetch_all(self):
        self.db_manager.insert("test_table", {"name": "Alice", "age": 30})
        self.db_manager.insert("test_table", {"name": "Bob", "age": 25})
        results = self.db_manager.fetch_all("test_table")
        self.assertEqual(len(results), 2)

    def test_fetch_where(self):
        self.db_manager.insert("test_table", {"name": "Alice", "age": 30})
        self.db_manager.insert("test_table", {"name": "Bob", "age": 25})
        results = self.db_manager.fetch_where("test_table", "age = ?", (25,))
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0][1], "Bob")

    def test_drop_table(self):
        self.db_manager.drop_table("test_table")
        tables = self.db_manager.fetch_query("SELECT name FROM sqlite_master WHERE type='table' AND name='test_table'")
        self.assertEqual(len(tables), 0)


if __name__ == "__main__":
    unittest.main()
