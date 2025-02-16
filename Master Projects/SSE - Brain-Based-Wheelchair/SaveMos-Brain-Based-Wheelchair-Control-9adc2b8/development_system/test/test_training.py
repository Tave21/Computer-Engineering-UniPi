"""
Module: test_training
Tests the training and validation phase.

Author: Gabriele Pianigiani

"""
from unittest import TestCase

import joblib

from development_system.classifier import Classifier
from development_system.trainer import Trainer



class TestTraining(TestCase):
    """Shows the learning plot"""
    # Generate a plot with the #iterations on the x-axis and the training error for each iteration on the y-axis
    # It takes as input an object of type LearningError, which stores the training error for each iteration
    def test_train_function(self):
        """
        Executes the train function and validates the returned classifier.

        This function ensures that the returned classifier object conforms to the
        expected Classifier type and has properly initialized attributes.

        Raises:
            AssertionError: If the returned classifier does not meet expectations.
        """
        # Execute the train function
        iterations = 100  # Example number of iterations
        validation = False #with false only train is checked, with true even the validate function
        trainer = Trainer()

        # Assuming `self` is an object that has a `train` method.
        trained_classifier = trainer.train(iterations=iterations, validation=validation)

        # Validate the classifier type
        assert isinstance(trained_classifier, Classifier), "The returned object is not of type 'Classifier'."

        # Validate the classifier attributes
        assert trained_classifier.get_num_iterations() == iterations, "Incorrect number of iterations in the classifier."
        assert trained_classifier.get_num_layers() is not None, "Number of layers is not set in the classifier."
        assert trained_classifier.get_num_neurons() is not None, "Number of neurons is not set in the classifier."

        # Additional validations for error attributes (optional)
        assert trained_classifier.get_training_error() is not None, "Training error is not set in the classifier."
        if validation:
            assert trained_classifier.get_validation_error() is not None, "Validation error is not set in the classifier."

        #save the testing classifier if all assertions pass
        joblib.dump(trained_classifier, "data/classifier.sav")
        # Print confirmation if all assertions pass
        print("Classifier has been successfully trained and validated.")
