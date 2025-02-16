import unittest
import os
from evaluation_system.Label import Label
from evaluation_system.LabelsBuffer import LabelsBuffer


class TestLabelsBuffer(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        """Create a test database file."""
        cls.db_name = "test_labels.db"
        cls.labels_buffer = LabelsBuffer(db_name=cls.db_name)

    @classmethod
    def tearDownClass(cls):
        """Clean up the test database."""
        try:
            # Ensure all database connections are closed
            import gc  # Garbage collector to force cleanup of SQLite objects
            gc.collect()  # Ensure all SQLite objects are released

            if os.path.exists(cls.db_name):
                os.remove(cls.db_name)
        except PermissionError as e:
            print(f"PermissionError: {e}. Make sure no process is using the file.")

    def setUp(self):
        """Ensure a clean state for each test."""
        self.labels_buffer.create_table()
        # Clear the database
        self.labels_buffer.execute_query("DELETE FROM labels")

    def test_create_table(self):
        """Test if the table is created successfully."""
        self.labels_buffer.create_table()
        result = self.labels_buffer.fetch_query(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='labels'")
        self.assertEqual(len(result), 1)

    def test_save_label(self):
        """Test saving a label to the database."""
        label = Label(uuid="1234", movements=2, expert=True)
        self.labels_buffer.save_label(label)
        result = self.labels_buffer.fetch_query("SELECT * FROM labels WHERE uuid=?", ("1234",))
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0], ("1234", 2, 1))

    def test_get_classifier_labels(self):
        """Test retrieving classifier labels."""
        label1 = Label(uuid="1234", movements=2, expert=False)
        label2 = Label(uuid="5678", movements=3, expert=False)
        self.labels_buffer.save_label(label1)
        self.labels_buffer.save_label(label2)
        labels = self.labels_buffer.get_classifier_labels(limit=1)
        self.assertEqual(len(labels), 1)
        self.assertEqual(labels[0].uuid, "1234")

    def test_get_expert_labels(self):
        """Test retrieving expert labels."""
        label1 = Label(uuid="1234", movements=2, expert=True)
        label2 = Label(uuid="5678", movements=3, expert=True)
        self.labels_buffer.save_label(label1)
        self.labels_buffer.save_label(label2)
        labels = self.labels_buffer.get_expert_labels(limit=2)
        self.assertEqual(len(labels), 2)
        self.assertEqual(labels[0].uuid, "1234")
        self.assertEqual(labels[1].uuid, "5678")

    def test_get_num_classifier_labels(self):
        """Test counting classifier labels."""
        label = Label(uuid="1234", movements=2, expert=False)
        self.labels_buffer.save_label(label)
        count = self.labels_buffer.get_num_classifier_labels()
        self.assertEqual(count, 1)

    def test_get_num_expert_labels(self):
        """Test counting expert labels."""
        label = Label(uuid="1234", movements=2, expert=True)
        self.labels_buffer.save_label(label)
        count = self.labels_buffer.get_num_expert_labels()
        self.assertEqual(count, 1)

    def test_delete_labels(self):
        """Test deleting labels."""
        label1 = Label(uuid="1234", movements=2, expert=True)
        label2 = Label(uuid="5678", movements=3, expert=False)
        self.labels_buffer.save_label(label1)
        self.labels_buffer.save_label(label2)
        self.labels_buffer.delete_labels(limit=1)
        expert_count = self.labels_buffer.get_num_expert_labels()
        classifier_count = self.labels_buffer.get_num_classifier_labels()
        self.assertEqual(expert_count, 0)
        self.assertEqual(classifier_count, 0)

    def test_duplicate_label(self):
        """Test handling duplicate label insertions."""
        label = Label(uuid="1234", movements=2, expert=True)
        self.labels_buffer.save_label(label)
        self.labels_buffer.save_label(label)  # Duplicate insert
        count = self.labels_buffer.get_num_expert_labels()
        self.assertEqual(count, 1)

    def test_empty_result(self):
        """Test fetching from an empty database."""
        labels = self.labels_buffer.get_classifier_labels(limit=5)
        self.assertEqual(len(labels), 0)
        count = self.labels_buffer.get_num_classifier_labels()
        self.assertEqual(count, 0)


if __name__ == '__main__':
    unittest.main()
