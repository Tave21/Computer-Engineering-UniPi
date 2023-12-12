from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from pandas import read_csv

data = read_csv('data/whole_data_16.csv')
datas = data[['x','y','z','E4_BVP','E4_EDA','E4_HR','E4_IBI','E4_TEMP','Attention','delta','lowAlpha','highAlpha','lowBeta','highBeta','lowGamma','middleGamma','theta','Meditation']]
#Scale the data
scaler = StandardScaler()
scaler.fit(datas)
scaled = scaler.transform(datas)

#Obtain principal components
pca = PCA().fit(scaled)

pc = pca.transform(scaled)
pc1 = pc[:,0]
pc2 = pc[:,1]

#Plot principal components
plt.figure(figsize=(10,10),dpi=100)

colour = []
for y in data['self_arousal']:
    if y == 1:
        colour.append('#FF0000')
    elif y == 2:
        colour.append('#0033FF')
    elif y == 3:
        colour.append('#FF9900')
    elif y == 4:
        colour.append('#008000')
    elif y == 5 :
        colour.append('#00F000')

plt.scatter(pc1,pc2 ,c=colour,edgecolors='#000000')
#plt.ylabel("Glucose",size=20)
#plt.xlabel('Age',size=20)
plt.yticks(size=12)
plt.xticks(size=12)
plt.xlabel('PC1')
plt.ylabel('PC2')
plt.savefig('plot.png')



var = pca.explained_variance_ratio_[0:10] #percentage of variance explained
labels = ['PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8','PC9','PC10']

plt.figure(figsize=(40,20))
plt.bar(labels,var,)
plt.xlabel('Pricipal Component')
plt.ylabel('Proportion of Variance Explained')
plt.savefig('barre.png')
