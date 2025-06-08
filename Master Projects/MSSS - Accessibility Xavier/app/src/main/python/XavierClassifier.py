import numpy as np
import joblib
import pandas as pd
from os.path import dirname,join
from scipy.signal import iirnotch, butter, filtfilt

from sklearn.ensemble import RandomForestClassifier
#from mindrove import NoiseTypes
#from mindrove.data_filter import DataFilter, FilterTypes, WindowOperations, DetrendOperations

fs=500
modello=None

def apply_notch_filter(x, fs, notch_freq=50.0, quality_factor=30.0):
    # Normalizziamo la frequenza rispetto a Nyquist
    w0 = notch_freq / (fs / 2)
    # Progettiamo i coefficienti del filtro
    b, a = iirnotch(w0, quality_factor)
    # Applichiamo il filtro in avanti e indietro per minimizzare la fase
    y = filtfilt(b, a, x)
    return y

def apply_highpass_filter(x, fs, cutoff=0.5, order=4):
    # Frequenza normalizzata di cutoff
    wn = cutoff / (fs / 2)
    # Progettiamo il filtro Butterworth passa-alto
    b, a = butter(order, wn, btype='highpass')
    # Applichiamo il filtro in avanti e indietro
    y = filtfilt(b, a, x)
    return y

def model_init():
    try:
        global modello
        nomeModel="XavierModel.sav"
        #salvati dove c'è lo script python
        model_path=join(dirname(__file__),nomeModel)
        #Caricamento modello
        modello = joblib.load(model_path)
        return True
    except Exception as e:
        print(f"Python [init]: Exception occurred: {str(e)}")
        return False  # Torna False in caso di errore per evitare crash


def EEG_classifier(buffer, n_canali):
    try:
        buffer = np.frombuffer(buffer, dtype=np.float64)
        if buffer.size % n_canali != 0:
            buffer = buffer[:(buffer.size // n_canali) * n_canali]

        window = buffer.reshape(-1, n_canali)
        #   500 campioni * 6 canali
        #   ch1|ch2|ch3|ch4|ch5|ch6
        #1
        #2
        #...
        #500
        #print("Inizio processazione dati")
        processed_channels = []
        for i in range(n_canali):
            ch=window[:,i].copy()
            #DataFilter.detrend(ch, DetrendOperations.CONSTANT.value)
            #DataFilter.remove_environmental_noise(ch,500,NoiseTypes.FIFTY)
            ch = apply_notch_filter(ch, fs)
            ch = apply_highpass_filter(ch, fs)
            processed_channels.append(ch)
        #print("Dati processati")
        tabella_numpy = np.column_stack(processed_channels)

        # Simulazione di finestra live (ad esempio 1 secondo a 500Hz = 500 campioni)
        colonne = ["CH1", "CH2", "CH3", "CH4", "CH5", "CH6"]
        X = pd.DataFrame(tabella_numpy, columns=colonne)

        # Calcola la media di tutte le righe di X come DataFrame con i nomi delle colonne
        media_riga = pd.DataFrame(X.mean(axis=0)).T
        media_riga.columns = colonne

        # Predizione
        binary_signal = modello.predict(media_riga)
        #print("Dati predetti")

        #situazione=1 #occhi chiusi
        print(binary_signal)
        if binary_signal.size > 0:
            situazione = np.argmax(np.bincount(binary_signal))
            #print("Moda calcolata")
        if situazione==1:
            print("Occhi aperti")
        else:
            print("Occhi chiusi")

        return situazione
    except Exception as e:
        print(f"Python [model]: Exception occurred: {str(e)}")
        return 0  # Torna False in caso di errore per evitare crash




