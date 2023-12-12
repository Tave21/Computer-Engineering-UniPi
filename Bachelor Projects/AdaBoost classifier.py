from sklearn.ensemble import AdaBoostClassifier
from pandas import read_csv
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import train_test_split
#import scikit-learn metrics module for accuracy calculation
from sklearn.metrics import accuracy_score
import numpy

data = read_csv('data/whole_data_16.csv')
X = data[['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']]
y = data['self_arousal']
le=LabelEncoder()
y=le.fit_transform(y)
# Split dataset into training set and test set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25)

# Create adaboost classifer object
abc = AdaBoostClassifier(n_estimators=50, learning_rate=1, random_state=0)

# Train Adaboost Classifer
model1 = abc.fit(X_train, y_train)

#Predict the response for test dataset
y_pred = model1.predict(X_test)

# calculate and print model accuracy
print("AdaBoost Classifier Model Accuracy:", accuracy_score(y_test, y_pred))

