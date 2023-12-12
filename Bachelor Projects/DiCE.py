
from sklearn.metrics import accuracy_score
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier
import matplotlib.pyplot as plt
import numpy as np
import random

# import some data to play with
data = pd.read_csv('whole_data_17.csv')

# we only take the first two features. We could avoid this ugly
# slicing by using a two-dim dataset
y = data['self_arousal']
X = data[['self_arousal','x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']]


train_dataset, test_dataset, y_train, y_test = train_test_split(X, y, test_size=0.25,random_state=42)
X_t = test_dataset[test_dataset['self_arousal']==2] #parto da 2
X_t =  X_t.drop('self_arousal',axis = 1)
X_train = train_dataset.drop('self_arousal',axis = 1)
X_test = test_dataset.drop('self_arousal',axis = 1)
query_instance = X_t[1:2]


#feature scaling
scaler = StandardScaler()
scaler.fit(X_train)

X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)


model= KNeighborsClassifier(n_neighbors=5,metric='minkowski') #default k = 5
model.fit(X_train, y_train)

import dice_ml

feature = ['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']
#target = "self_arousal"
d = dice_ml.Data(dataframe=train_dataset, continuous_features=feature, outcome_name= "self_arousal")
m = dice_ml.Model(model=model, backend="sklearn")

exp = dice_ml.Dice(d, m, method="random")

#print(query_instance)

query_instance.to_csv('campione.csv', index = False)
e1 = exp.generate_counterfactuals(query_instance, total_CFs=10,desired_class= 3)
e1.cf_examples_list[0].final_cfs_df.to_csv(path_or_buf="result.csv",index =False)

imp = exp.local_feature_importance(query_instance, cf_examples_list=e1.cf_examples_list)
print(imp.local_importance)
query_instance = X_t[0:100]
cobj = exp.global_feature_importance(query_instance, total_CFs=10, desired_class=3, posthoc_sparsity_param=None)
cobj.cf_examples_list[0].final_cfs_df.to_csv(path_or_buf="result.csv",index =False)
print(cobj.summary_importance)