from sklearn.preprocessing import StandardScaler
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.manifold import TSNE

df = pd.read_csv('whole_data_16.csv')
print(df.head(4))

l = df['self_arousal']

d = df[['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']]

standardized_data = StandardScaler().fit_transform(d)
print(standardized_data.shape)


# TSNE
# Picking the top 1000 points as TSNE
# takes a lot of time for 15K points
data_1000 = standardized_data[0:60480, :]
labels_1000 = l[0:60480]

model = TSNE(n_components = 2, random_state = 0)
# configuring the parameters
# the number of components = 2
# default perplexity = 30
# default learning rate = 200
# default Maximum number of iterations
# for the optimization = 1000

tsne_data = model.fit_transform(data_1000)

# creating a new data frame which
# help us in plotting the result data
tsne_data = np.vstack((tsne_data.T, labels_1000)).T
tsne_df = pd.DataFrame(data = tsne_data, columns =("Dim_1", "Dim_2", "self_arousal"))

# Plotting the result of tsne

sns.FacetGrid(tsne_df, hue ="self_arousal").map(plt.scatter, 'Dim_1', 'Dim_2').add_legend()
plt.show()