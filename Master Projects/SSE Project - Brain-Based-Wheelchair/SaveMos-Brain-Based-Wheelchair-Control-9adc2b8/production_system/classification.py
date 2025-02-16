"""
Author: Alessandro Ascani
"""
import joblib
import pandas as pd
from sklearn.neural_network import MLPClassifier
from production_system.label import Label




class Classification:
    """
     Class that managing the classifier executing the deployment and classification operation

    """
    def __init__(self):
        self._classifier: MLPClassifier or None = None



    def classify(self, prepared_session, classifier_deployed):
        """
        Method that execute classify operation based on received prepared_session and classifier
        Args:
            prepared_session: prepared session that must be classified
            classifier_deployed: flag that say if a classifier was deployed in a previous deployment session

        Returns:
            label: an object of label class representing the label obtained from classify operation

        """

        if classifier_deployed is False:
            return None

        # convert prepared session json in python object
        if self._classifier is None:
            self._classifier = joblib.load("model/classifier.sav")


        environment_mapping = {"slippery":0, "plain":1, "slope":2, "house":3, "track":4}
        activity_mapping = {"shopping":0, "sport":1, "cooking":2, "relax":3, "gaming":4}

        # convert features in Data Frame
        features_struct = {
            'psd_alpha_band': prepared_session['psd_alpha_band'],
            'psd_beta_band': prepared_session['psd_beta_band'],
            'psd_theta_band': prepared_session['psd_theta_band'],
            'psd_delta_band': prepared_session['psd_delta_band'],
            'activity': activity_mapping.get(prepared_session['activity']),
            'environment': environment_mapping.get(prepared_session['environment'])
        }

        features_DF = pd.DataFrame([features_struct])

        label_identified = self._classifier.predict(features_DF)
        print(label_identified)
        label_mapping = {
            0: "turnRight",
            1: "turnLeft",
            2: "move"
        }

        # Set values for the label json
        movement = label_mapping.get(int(round(float(label_identified.flatten()[0]))), None)
        label = Label(prepared_session['uuid'], movement)


        return label


