from pandas import read_csv
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import accuracy_score
from numpy import ravel
# Read csv
data = read_csv('data/whole_data_16.csv')
data = data.drop("self_valence",axis=1)
# Random split 75% as training and 25% as testing
training_data, testing_data, training_labels, testing_labels = train_test_split(data.iloc[:, 7:25], data.iloc[:, 5])
# Save as csv
training_data.to_csv('data/trainingData.csv', index=False)
testing_data.to_csv('data/testingData.csv', index=False)
training_labels.to_csv('data/trainingLabels.csv', index=False)
testing_labels.to_csv('data/testingLabels.csv', index=False)

training_data = read_csv('data/trainingData.csv')
training_labels = read_csv('data/trainingLabels.csv')
testing_data = read_csv('data/testingData.csv')
testing_labels = read_csv('data/testingLabels.csv')
# Build and train the classifier
mlp = MLPClassifier()
mlpmodel = mlp.fit(training_data,ravel(training_labels))
mlplabels = mlpmodel.predict(testing_data)
mlpscore = accuracy_score(ravel(testing_labels),mlplabels)
print("MLP score: ", mlpscore)