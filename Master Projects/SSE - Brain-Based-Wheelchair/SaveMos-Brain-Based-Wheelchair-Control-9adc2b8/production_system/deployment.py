"""
Author: Alessandro Ascani
"""

class Deployment:
    """
     Class that execute deployment operation
    """

    @staticmethod
    def deploy(classifier):
        """
        Saves the provided classifier in a .sav file
        Args:
            classifier: file with classifier in binary format to save
        """
        try:
            binary_content = classifier.encode('latin1')
            with  open("model/classifier.sav", "wb") as f:
                f.write(binary_content)

            return True

        except (UnicodeEncodeError, IOError) as e:
            return False
