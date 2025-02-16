import unittest

from segregation_system.learning_set import LearningSet
from segregation_system.test.test_utility_lib import generate_random_prepared_sessions_object_list
from utility.json_handler.json_handler import JsonHandler


class TestLearningSet(unittest.TestCase):
    def setUp(self):
        """
        Sets up test data for the tests.
        """
        self.training_sessions = generate_random_prepared_sessions_object_list(5)
        self.validation_sessions = generate_random_prepared_sessions_object_list(4)
        self.test_sessions = generate_random_prepared_sessions_object_list(2)

        self.learning_set = LearningSet(
            self.training_sessions,
            self.validation_sessions,
            self.test_sessions
        )

    def test_to_dict(self):
        """
        Tests the to_dict method of the LearningSet class.
        """
        learning_set_dict = self.learning_set.to_dict()

        # Assert dictionary structure
        self.assertIn("training_set", learning_set_dict, "The dictionary should contain 'training_set'.")
        self.assertIn("validation_set", learning_set_dict, "The dictionary should contain 'validation_set'.")
        self.assertIn("test_set", learning_set_dict, "The dictionary should contain 'test_set'.")

        # Assert correct serialization of training sessions
        self.assertEqual(len(learning_set_dict["training_set"]), len(self.training_sessions),
                         "The number of training sessions in the dictionary should match.")

        for i, session in enumerate(learning_set_dict["training_set"]):
            self.assertEqual(session["uuid"], self.training_sessions[i].uuid, "UUID mismatch.")
            self.assertEqual(session["psd_alpha_band"], self.training_sessions[i].features[0], "Alpha band mismatch.")
            self.assertEqual(session["psd_beta_band"], self.training_sessions[i].features[1], "Beta band mismatch.")
            self.assertEqual(session["psd_theta_band"], self.training_sessions[i].features[2], "Theta band mismatch.")
            self.assertEqual(session["psd_delta_band"], self.training_sessions[i].features[3], "Delta band mismatch.")
            self.assertEqual(session["activity"], self.training_sessions[i].features[4], "Activity mismatch.")
            self.assertEqual(session["environment"], self.training_sessions[i].features[5], "Environment mismatch.")
            self.assertEqual(session["label"], self.training_sessions[i].label, "Label mismatch.")

    def test_conversion_round_trip(self):
        """
        Test conversion from LearningSet to dict, to string, and back to LearningSet.
        Verifies that the conversion is correct at each stage.
        """
        # Step 1: Convert LearningSet to dict
        learning_set_dict = self.learning_set.to_dict()

        # Step 2: Convert dict to string
        dict_str = JsonHandler.dict_to_string(learning_set_dict)

        # Step 3: Convert string back to dict
        dict_from_str = JsonHandler.string_to_dict(dict_str)

        # Step 4: Reconstruct LearningSet from dict
        new_learning_set = LearningSet.from_dict(dict_from_str)

        # Step 5: Verify that the reconstructed LearningSet matches the original one
        self.assertEqual(len(new_learning_set.training_set), len(self.learning_set.training_set),
                         "The number of training sessions in the reconstructed LearningSet should match.")
        self.assertEqual(len(new_learning_set.validation_set), len(self.learning_set.validation_set),
                         "The number of validation sessions in the reconstructed LearningSet should match.")
        self.assertEqual(len(new_learning_set.test_set), len(self.learning_set.test_set),
                         "The number of test sessions in the reconstructed LearningSet should match.")

        # Assert equality of each session after the round trip
        for i in range(len(new_learning_set.training_set)):
            self.assertEqual(new_learning_set.training_set[i], self.learning_set.training_set[i],
                             f"Training session {i} does not match after round-trip conversion.")
        for i in range(len(new_learning_set.validation_set)):
            self.assertEqual(new_learning_set.validation_set[i], self.learning_set.validation_set[i],
                             f"Validation session {i} does not match after round-trip conversion.")
        for i in range(len(new_learning_set.test_set)):
            self.assertEqual(new_learning_set.test_set[i], self.learning_set.test_set[i],
                             f"Test session {i} does not match after round-trip conversion.")
