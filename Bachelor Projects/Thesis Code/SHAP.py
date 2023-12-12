from pandas import read_csv
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier
import numpy as np
import matplotlib.pyplot as plt

# import some data to play with
data = read_csv('whole_data_17.csv')

# we only take the first two features. We could avoid this ugly
# slicing by using a two-dim dataset
y = data['self_arousal']
X = data[['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']]


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25)
tes = X_train
test = X_test
print(tes.mean())
#feature scaling
scaler = StandardScaler()
scaler.fit(X_train)

X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)


rf = KNeighborsClassifier(n_neighbors=5,metric='minkowski') #default k = 5
rf.fit(X_train, y_train)
feature = ['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']
import shap

rf_shap_values = shap.KernelExplainer(rf.predict,X_test[0:30]).shap_values(X_train[0:30])

shap.summary_plot(rf_shap_values, X_train[0:30],feature_names=feature,show=False,plot_type="dot")
plt.savefig("shap_variable_importance.png")
plt.close()
shap.summary_plot(rf_shap_values, X_train[0:30], plot_type="bar",feature_names=feature,show=False)
plt.savefig("variable_importance.png")
plt.close()
shap.dependence_plot(5,rf_shap_values,X_train[0:30],feature_names=feature)
plt.savefig("dependence_plot.png")
shap.dependence_plot(8,rf_shap_values,X_train[0:30],feature_names=feature)
plt.savefig("dependence_plot_2.png")

shap.initjs()
S = test.copy()
explainerModel = shap.KernelExplainer(rf.predict, test[0:7])
rf_shap_values = explainerModel.shap_values(tes[0:7])
shap.force_plot(explainerModel.expected_value, rf_shap_values[2,:], S.iloc[2,:],matplotlib=True,show = False)
plt.savefig("sr.png",bbox_inches='tight', dpi=600)
plt.close()
shap.force_plot(explainerModel.expected_value, rf_shap_values[3,:], S.iloc[3,:],matplotlib=True,show = False)
plt.savefig("sr3.png",bbox_inches='tight', dpi=600)
plt.close()

shap.force_plot(explainerModel.expected_value, rf_shap_values[4,:], S.iloc[4,:],matplotlib=True,show = False)
plt.savefig("sr1.png",bbox_inches='tight', dpi=600)
plt.close()
shap.force_plot(explainerModel.expected_value, rf_shap_values[5,:], S.iloc[5,:],matplotlib=True,show = False)
plt.savefig("sr2.png",bbox_inches='tight', dpi=600)
plt.close()
shap.force_plot(explainerModel.expected_value, rf_shap_values[6,:], S.iloc[6,:],matplotlib=True,show = False)
plt.savefig("sr4.png",bbox_inches='tight', dpi=600)
plt.close()
shap.force_plot(explainerModel.expected_value, rf_shap_values[1,:], S.iloc[1,:],matplotlib=True,show = False)
plt.savefig("sr5.png",bbox_inches='tight', dpi=600)
plt.close()

shap.force_plot(explainerModel.expected_value, rf_shap_values[0,:], S.iloc[0,:],matplotlib=True,show = False)
plt.savefig("sr6.png",bbox_inches='tight', dpi=600)
plt.close()




