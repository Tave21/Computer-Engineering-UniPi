import unittest
from unittest.mock import patch, mock_open, MagicMock
from production_system.deployment import Deployment

class TestDeployment(unittest.TestCase):

    @patch("builtins.open", new_callable=mock_open)
    def test_deploy_successful(self, mock_file):
        """
        Test che verifica il comportamento del metodo deploy con un classificatore valido.
        """
        # Mock del classificatore
        mock_classifier = MagicMock()
        mock_classifier.encoded.return_value = b"mock_binary_content"

        # Chiamata al metodo deploy
        result = Deployment.deploy(mock_classifier)

        # Verifica se il risultato è True
        self.assertTrue(result)

        # Verifica se il file è stato scritto correttamente
        mock_file.assert_called_once_with("model/classifier.sav", "wb")
        mock_file().write.assert_called_once_with(b"mock_binary_content")

    def test_deploy_encoding_failure(self):
        """
        Test che verifica il comportamento del metodo deploy se l'encoding fallisce.
        """
        # Mock del classificatore che genera un errore durante l'encoding
        mock_classifier = MagicMock()
        mock_classifier.encoded.side_effect = UnicodeEncodeError("latin1", "", 0, 1, "mock_error")

        # Chiamata al metodo deploy
        result = Deployment.deploy(mock_classifier)

        # Verifica se il risultato è False
        self.assertFalse(result)

    @patch("builtins.open", new_callable=mock_open)
    def test_deploy_io_error(self, mock_file):
        """
        Test che verifica il comportamento del metodo deploy se c'è un IOError.
        """
        # Mock del classificatore
        mock_classifier = MagicMock()
        mock_classifier.encoded.return_value = b"mock_binary_content"

        # Simula un IOError durante l'apertura del file
        mock_file.side_effect = IOError("Errore di scrittura")

        # Chiamata al metodo deploy
        result = Deployment.deploy(mock_classifier)

        # Verifica se il risultato è False
        self.assertFalse(result)

if __name__ == "__main__":
    unittest.main()
