from pandas import read_csv
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier


# import some data to play with
data = read_csv('data/whole_data_16.csv')

# we only take the first two features. We could avoid this ugly
# slicing by using a two-dim dataset
y = data['self_arousal']
X = data[['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']]


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25)
#feature scaling
scaler = StandardScaler()
scaler.fit(X_train)

X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)


classifier = KNeighborsClassifier(n_neighbors=5,metric='minkowski') #default k = 5
classifier.fit(X_train, y_train)

#make prediction
y_pred = classifier.predict(X_test)


print("KNN Classifier Model Accuracy:", accuracy_score(y_test, y_pred))