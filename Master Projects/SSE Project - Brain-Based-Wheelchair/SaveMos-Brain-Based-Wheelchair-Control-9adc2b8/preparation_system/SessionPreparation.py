"""
Module: SessionPreparation
Prepare session correcting samples, outliers and extracting features
Author: Francesco Taverna
"""
import json
from typing import Union

import numpy as np
from mne.time_frequency.multitaper import psd_array_multitaper
from scipy.integrate import simps



class SessionPreparation:

    def correct_missing_samples(self, raw_session: dict, placeholder: Union[int, str, None]) -> dict:
        """
        corrects the missing samples with an interpolation function
        :param raw_session: raw session
        :param placeholder: missing value to replace
        :return: False if records are missing, the raw session otherwise (as dict)
        """

        # Find indices of null values in the list
        null_indices = [i for i, value in enumerate(raw_session['eeg_data']) if value is placeholder]

        # Create an array of indices for the non-null values
        other_indices = [i for i in range(len(raw_session['eeg_data'])) if i not in null_indices]

        # Perform linear interpolation for each null value and update the list in place
        for null_index in null_indices:
            # Use numpy.interp for linear interpolation
            # for interpolating it is necessary to use two sets of points:
            # x is the set of points indexes with valid value
            # y is the sets of corresponding values of indexes in x
            # then the function computes the mid point null_index
            interpolated_value = np.interp(null_index, other_indices, [raw_session['eeg_data'][i] for i in other_indices])

            # Update the value in the original data list
            raw_session['eeg_data'][null_index] = interpolated_value

        return raw_session

    def correct_outliers(self, raw_session: dict) -> dict:
        """
        corrects outliers using the value_range.
        :param raw_session: raw_session
        :return: the corrected Raw session
        """
        #hardcoded values
        min_value = -27.3024593023448 #computed using upper_bound.py in data folder, 2 percentile of all data in helmet.csv
        max_value = 28.170070834680384 #computed using upper_bound.py in data folder, 98 percentile of all data in helmet.csv

        # bound between min and max
        raw_session['eeg_data'] = [min(max(value, min_value), max_value) for value in raw_session['eeg_data']]
        #if activity is not defined insert last value
        if(raw_session['activity'] == "null"):
            try:
                with open("last_activity.json" , "r") as file:
                    raw_session['activity'] = json.load(file)['activity']
            except FileNotFoundError:
                print("File not found")

        #if environment is not defined insert last value
        if(raw_session['environment'] == "null"):
            try:
                with open("last_environment.json", "r") as file:
                    raw_session['environment'] = json.load(file)['environment']
            except FileNotFoundError:
                print("File not found")

        return raw_session

    def extract_feature(self, time_series: np.array, sf: float, band: list, relative=False) -> float:
        """Compute the average power of the signal x in a specific frequency band.
            Parameters:
                time_series: 1-d array
                    Input signal in time-domain
                sf : float
                    sample frequency of the data
                band: list
                    lower and upper frequencies of the band of interest.
                relative: boolean
                    If True, return the relative power ( = divided by the total power of the signal).
                    If False (default), return the absolute power
            Return:
                bp: float
                    Absolute or relative band power
        """
        band = np.asarray(band)
        low, high = band
        psd, frequencies = psd_array_multitaper(time_series, sf, adaptive=True, normalization='full', verbose=0)

        # Frequency resolution
        freq_res = frequencies[1] - frequencies[0]

        # Find index of band in frequency vector
        idx_band = np.logical_and(frequencies >= low, frequencies <= high)

        # Integral approximation of the spectrum using parabola (Simpson's rule)
        bp = simps(psd[idx_band], dx=freq_res)
        if relative:
            bp /= simps(psd, dx=freq_res)

        return bp

    def create_prepared_session(self, raw_session: dict) -> dict:
            prepared_session = {
                "uuid": raw_session["uuid"],
                "label": raw_session["label"],
            }

            #create a numpy array
            time_series = np.array(raw_session['eeg_data'])

            #four main bandwidths to extract from eeg_data (hardcoded)
            bandwidths = {
                "psd_alpha_band": [8, 12],
                "psd_beta_band": [12, 30],
                "psd_theta_band": [1, 4],
                "psd_delta_band": [4, 8]
            }
            sampling_frequency = 100.0 #taken from EEG website

            for band, range_values in bandwidths.items():
                prepared_session[band] = self.extract_feature(
                    time_series,
                    sampling_frequency,
                    range_values
                )

            prepared_session["activity"] = raw_session["activity"]
            prepared_session["environment"] = raw_session["environment"]

            return prepared_session
