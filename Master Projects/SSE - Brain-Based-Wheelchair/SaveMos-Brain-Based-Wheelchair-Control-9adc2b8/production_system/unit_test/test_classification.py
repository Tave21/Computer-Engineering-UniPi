import unittest
import pandas as pd
from unittest.mock import patch, MagicMock
from production_system.classification import Classification
from production_system.label import Label


class TestClassification(unittest.TestCase):

    @patch('production_system.classification.joblib.load')
    def test_classify_valid_prepared_session(self, mock_joblib_load):
        # Mock the classifier loaded by joblib
        mock_classifier = MagicMock()
        mock_classifier.predict.return_value = ["movement_label"]
        mock_joblib_load.return_value = mock_classifier

        # Test data
        prepared_session_data = {
            "uuid": "001",
            "PSD_alpha_band": 0.8,
            "PSD_beta_band": 0.7,
            "PSD_theta_band": 0.9,
            "PSD_delta_band": 0.6,
            "activity": "cooking",
            "environment": "house",
            "label": "turnRight"
        }

        # Instantiate the classification class
        classification_instance = Classification()

        # Call the classify method
        label = classification_instance.classify(prepared_session_data)

        # Assertions
        self.assertIsInstance(label, Label)
        self.assertEqual(label.uuid, "001")
        self.assertEqual(label.movements, ["movement_label"])

        # Check that the classifier's predict method was called with the correct input
        expected_features_struct = {
            'PSD_alpha_band': 0.8,
            'PSD_beta_band': 0.7,
            'PSD_theta_band': 0.9,
            'PSD_delta_band': 0.6,
            'activity': 2,
            'environment': 3,
            'label': 0
        }
        expected_features = pd.DataFrame([expected_features_struct])

        # Recupera gli argomenti passati alla chiamata
        args, kwargs = mock_classifier.predict.call_args
        passed_features = args[0]  # Il primo argomento della chiamata

        # Verifica l'uguaglianza dei DataFrame
        pd.testing.assert_frame_equal(expected_features, passed_features)

    @patch('production_system.classification.joblib.load')
    def test_classify_without_preloaded_classifier(self, mock_joblib_load):
        # Mock joblib to ensure classifier is loaded
        mock_classifier = MagicMock()
        mock_classifier.predict.return_value = ["test_movement"]
        mock_joblib_load.return_value = mock_classifier

        # Test data
        prepared_session_data = {
            "uuid": "002",
            "PSD_alpha_band": 0.1,
            "PSD_beta_band": 0.2,
            "PSD_theta_band": 0.3,
            "PSD_delta_band": 0.4,
            "activity": "gaming",
            "environment": "slope",
            "label": "move"
        }

        classification_instance = Classification()
        label = classification_instance.classify(prepared_session_data)

        self.assertEqual(label.uuid, "002")
        self.assertEqual(label.movements, ["test_movement"])

        mock_joblib_load.assert_called_once_with("model/classifier.sav")

    @patch('production_system.classification.joblib.load')
    def test_classify_with_preloaded_classifier(self, mock_joblib_load):
        # Preload a mock classifier
        mock_classifier = MagicMock()
        mock_classifier.predict.return_value = ["preloaded_movement"]
        classification_instance = Classification()
        classification_instance._classifier = mock_classifier  # Set the preloaded classifier

        # Test data
        prepared_session_data = {
            "uuid": "003",
            "PSD_alpha_band": 0.5,
            "PSD_beta_band": 0.6,
            "PSD_theta_band": 0.7,
            "PSD_delta_band": 0.8,
            "activity": "relax",
            "environment": "track",
            "label": "move"
        }

        label = classification_instance.classify(prepared_session_data)

        self.assertEqual(label.uuid, "003")
        self.assertEqual(label.movements, ["preloaded_movement"])

        # joblib.load should not be called since the classifier is preloaded
        mock_joblib_load.assert_not_called()

if __name__ == '__main__':
    unittest.main()
