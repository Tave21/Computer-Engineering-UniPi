import pandas as pd
import os


def calculate_total_98th_percentile(file_name):
    """
    Calcola il 98º percentile di tutti i valori numerici presenti in un file CSV.

    Args:
        file_name (str): Nome del file CSV.

    Returns:
        float: Il valore del 98º percentile considerando tutti i dati numerici.
    """
    if not os.path.exists(file_name):
        raise FileNotFoundError(f"Il file '{file_name}' non esiste nella directory corrente.")

    # Legge il file CSV
    data = pd.read_csv(file_name)

    # Seleziona solo le colonne numeriche
    numeric_data = data.select_dtypes(include=['number'])

    if numeric_data.empty:
        raise ValueError("Il file CSV non contiene colonne numeriche.")

    # Combina tutti i valori numerici in un unico array
    all_numeric_values = numeric_data.values.flatten()

    # Rimuove eventuali valori NaN
    all_numeric_values = all_numeric_values[~pd.isna(all_numeric_values)]

    # Calcola il 98º percentile
    percentile_98 = pd.Series(all_numeric_values).quantile(0.02) #0.98 for upper_bound, 0.02 for lower_bound

    return percentile_98


if __name__ == "__main__":
    file_name = "helmet.csv"

    try:
        result = calculate_total_98th_percentile(file_name)
        print(f"Il 98º percentile di tutti i valori numerici è: {result}")
    except Exception as e:
        print(f"Errore: {e}")
