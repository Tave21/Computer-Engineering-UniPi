"""
Module: raw_session_preparation
Handles the preparation of raw sessions from records.

Author: Francesco Taverna

"""
import math

from .raw_session import RawSession

class RawSessionPreparation:
    """
    Prepares raw sessions and marks missing samples in the records.
    """

    def create_raw_session(self, records: list):
        """
        Create a raw session from the given records.

        Args:
            records (list): List of records to include in the raw session.

        Returns:
            RawSession: A new raw session instance.
        """
        uuid = records[0]
        environment = records[1]
        label = records[2]
        eeg_data = records[3]
        activity = records[4]

        return RawSession(uuid, environment, eeg_data, activity, label)

    def mark_missing_samples(self, raw_session, placeholder):
        """
        Mark missing samples in the raw session's EEG data.

        Args:
            raw_session (RawSession): The raw session to process.
            placeholder : Placeholder to set in NaN place
        Returns:
            number of missing samples
            rawsession updated (RawSession)
        """
        missing_samples = 0
        for index, value in enumerate(raw_session.eeg_data):
            #check if the value is a float and a NaN
            if isinstance(value, float) and math.isnan(value):
                #set placeholder in NaN place
                raw_session.eeg_data[index] = placeholder
                #increase the number of missing samples
                missing_samples+= 1

        return missing_samples, raw_session

