import unittest

from segregation_system.learning_set_splitter import LearningSetSplitter
from segregation_system.segregation_system_parameters import SegregationSystemConfiguration
from segregation_system.test.test_utility_lib import generate_random_prepared_sessions_object_list


class TestLearningSetSplitter(unittest.TestCase):
    """Unit test for LearningSetSplitter."""

    def setUp(self):
        """
        Set up the test case with a configuration and mock data.
        """
        SegregationSystemConfiguration.configure_parameters("conf/segregation_system_configuration.json" , "conf/global_netconf.json")


        # Initialize the LearningSetSplitter with the mock configuration
        self.splitter = LearningSetSplitter()

        # Generate 100 PreparedSession objects with random UUIDs
        self.prepared_sessions = generate_random_prepared_sessions_object_list(100)

    def test_generateLearningSets(self):
        """
        Test if the `generateLearningSets` method correctly splits the sessions into
        training, validation, and test sets with the correct proportions.
        """
        # Generate the learning sets
        learning_sets = self.splitter.generateLearningSets(self.prepared_sessions)

        # Get the individual sets
        training_set = learning_sets.training_set
        validation_set = learning_sets.validation_set
        test_set = learning_sets.test_set

        # Check the number of sessions in each set
        total_sessions = len(self.prepared_sessions)
        expected_training_count = int(total_sessions * 0.7)  # 70%
        expected_validation_count = int(total_sessions * 0.2)  # 20%
        expected_test_count = int(total_sessions * 0.1)  # 10%

        # Assert that the sets have the correct number of sessions
        self.assertEqual(len(training_set), expected_training_count,
                         f"Expected {expected_training_count} sessions in the training set, but got {len(training_set)}.")
        self.assertEqual(len(validation_set), expected_validation_count,
                         f"Expected {expected_validation_count} sessions in the validation set, but got {len(validation_set)}.")
        self.assertEqual(len(test_set), expected_test_count,
                         f"Expected {expected_test_count} sessions in the test set, but got {len(test_set)}.")

        # Optionally, you can check that the total number of sessions is equal to 100
        self.assertEqual(len(training_set) + len(validation_set) + len(test_set), total_sessions,
                         "The total number of sessions in the splits does not match the original number of sessions.")


if __name__ == "__main__":
    unittest.main()
