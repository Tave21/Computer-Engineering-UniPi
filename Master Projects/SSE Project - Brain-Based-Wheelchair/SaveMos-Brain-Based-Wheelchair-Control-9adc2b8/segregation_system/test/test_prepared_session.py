import unittest

from segregation_system.prepared_session import PreparedSession
from segregation_system.test.test_utility_lib import generate_random_prepared_session_object
from utility.json_handler.json_handler import JsonHandler


class TestPreparedSession(unittest.TestCase):
    def setUp(self):
        """
        This method is called before every test.
        """
        self.test_data = generate_random_prepared_session_object().to_dictionary()

        # Create a session object using from_dict
        self.session = PreparedSession(
            uuid=self.test_data['uuid'],
            features=[
                self.test_data['psd_alpha_band'],
                self.test_data['psd_beta_band'],
                self.test_data['psd_theta_band'],
                self.test_data['psd_delta_band'],
                self.test_data['activity'],
                self.test_data['environment']
            ],
            label=self.test_data['label']
        )

    def test_to_dictionary(self):
        """
        Test the to_dictionary method to ensure it correctly converts an object to a dictionary.
        """
        result = self.session.to_dictionary()

        # Check if the dictionary contains the correct values
        self.assertEqual(result['uuid'], self.test_data['uuid'])
        self.assertEqual(result['label'], self.test_data['label'])
        self.assertEqual(result['psd_alpha_band'], self.test_data['psd_alpha_band'])
        self.assertEqual(result['psd_beta_band'], self.test_data['psd_beta_band'])
        self.assertEqual(result['psd_theta_band'], self.test_data['psd_theta_band'])
        self.assertEqual(result['psd_delta_band'], self.test_data['psd_delta_band'])
        self.assertEqual(result['activity'], self.test_data['activity'])
        self.assertEqual(result['environment'], self.test_data['environment'])

    def test_conversion_round_trip(self):
        """
        Test conversion from PreparedSession to dict, to string, and back to PreparedSession.
        Verifies that the conversion is correct at each stage.
        """
        # Step 1: Convert PreparedSession to dict
        session_dict = self.session.to_dictionary()

        # Step 2: Convert dict to string
        dict_str = JsonHandler.dict_to_string(session_dict)

        # Step 3: Convert string back to dict
        dict_from_str = JsonHandler.string_to_dict(dict_str)

        # Step 4: Reconstruct PreparedSession from dict
        new_session = PreparedSession.from_dictionary(dict_from_str)

        # Check if the session object has been correctly initialized
        self.assertEqual(new_session._uuid, self.test_data['uuid'])
        self.assertEqual(new_session._features, [
            self.test_data['psd_alpha_band'],
            self.test_data['psd_beta_band'],
            self.test_data['psd_theta_band'],
            self.test_data['psd_delta_band'],
            self.test_data['activity'],
            self.test_data['environment']
        ])
        self.assertEqual(new_session._label, self.test_data['label'])

if __name__ == '__main__':
    unittest.main()
