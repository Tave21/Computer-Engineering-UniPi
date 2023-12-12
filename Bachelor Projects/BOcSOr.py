import os
import numpy as np
from numpy import ravel
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier
from CounterfactualExplainerByProximity import CounterfactualExplainerByProximity
#from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier

#from CounterfactualExplainerWithSHAP import CounterfactualExplainerWithSHAP
#from CounterfactualExplainerWithFI import CounterfactualExplainerWithFI
#from UCIUtils import UCIDatasetLoader

if __name__ == "__main__":
    current_path = "C:\\Users\\39334\Desktop\Tesi\DELIVERY"  # insert full local path

    data = pd.read_csv(current_path + "\whole_data_17.csv")#.drop(['unnamed 0'],axis=1)# insert dataset name on local path
    #data = data.drop("self_valence",axis=1)
    # Create histogram on MEDV column (target column)

    import matplotlib.pyplot as plt
    import matplotlib

    matplotlib.rcParams['font.size'] = 10
    matplotlib.rcParams['figure.dpi'] = 100

    from IPython.core.pylabtools import figsize
    figsize(7, 5)

    plt.hist(data['self_arousal'], color='blue', edgecolor='black', bins=int(45 / 1))

    plt.xlabel('nome classi')
    plt.ylabel('num di volte')
    plt.title('label self arousal')
    #plt.show()
    #da = da.to_numpy()
    #print(da)
    #print(da)
    #da = data.iloc[:,5]
    #da = data[['self_arousal']].values
    #da = da.reset_index(drop=True)
    #da = data['self_arousal'].values
    #print(da)

    #da = da.to_string(index = False)
   # dat = dat.to_numpy()
   # dat = dat.to_string(index = False)

    #y = data['self_arousal']
    #X = data[['x', 'y', 'z', 'E4_BVP', 'E4_EDA', 'E4_HR', 'E4_IBI', 'E4_TEMP', 'Attention', 'delta', 'lowAlpha', 'highAlpha',
    #     'lowBeta', 'highBeta', 'lowGamma', 'middleGamma', 'theta', 'Meditation']]
    #print(dat)
    #print(data.iloc[:,7:25])

    train_features, test_features, train_labels, test_labels = train_test_split(data.iloc[:,7:25], data.iloc[:,5], test_size=0.1) # insert correct column indexes to separate labels and features


    #train_features, test_features, train_labels, test_labels = train_test_split(X, y, test_size=0.1)
    """
    # Alternative with benchmark datasets
    dataset_names = ["car", "ecoli", "letter", "penbased", "satimage", "segment", "splice", "vowel"]

    for d in range(0, len(dataset_names)):
        train_features, test_features, train_labels, test_labels, classes_number = UCIDatasetLoader.uci_dataset_handler(
            dataset_names[d], test_split=0.1)
    """

    model = MLPClassifier(hidden_layer_sizes=(128, 64, 32)).fit(train_features.values, ravel(train_labels.values)) # you can choose different classifier from sklearn https://scikit-learn.org/stable/supervised_learning.html
    #model =  KNeighborsClassifier(n_neighbors=5,metric='minkowski').fit(train_features, train_labels)
    #print(temp)
    #print(temp2)
    c = CounterfactualExplainerByProximity(x=train_features, y=train_labels, model=model, original_class=2, counterfactual_class=4)
    c.explain_decision_boundary(perc_threshold=10, k_neighbors=10, n_steps=10, save_on_file=True, base_path=current_path + "/results/")
    colnames = ['index','sample','val','feature','score','itself','in','to','fin','chang','inii','too','fini']
    datas = pd.read_csv(current_path + "\\results\_single_features_switch.csv",names = colnames,header = None)#.drop(['unnamed 0'],axis=1)# insert dataset name on local path
    # counting unique values
    arr = pd.unique(datas.iloc[:,4])
    n = len(arr)

    print("No.of.unique values :",
          n)
    for numero in range(18):
        if numero in arr:
            x = datas['score'].value_counts()[numero]
            x = (x / len(datas.index)) * 100
            print(numero," = ",x,"%")



    """
    # leave this lines commented ... we will use it later on
    c.get_boundary_info(perc_threshold=5, save_to_file=True, base_path=current_path + "results/")   
    try:
            os.remove(current_path + "results/tmp/4.json")
        except OSError as e:
            print(e)
        else:
            print("File JSON about MCFI is deleted successfully")
    """

    