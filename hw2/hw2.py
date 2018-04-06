# -*- coding: utf-8 -*-
from sklearn import decomposition
import matplotlib.pyplot as pylot
from sklearn.manifold import TSNE

file = open('100_word_vector.txt', 'r')
line = file.readline()  #read the file

word2vector = {}
dim_list = []
while(line != ""):
	word_dim = line.split('\t')  #split string
	word = word_dim[0]  #word
	dim = word_dim[1][:-2]  #100 dim 
	dim = dim.split(' ')
	dim = list(map(lambda x : float(x), dim))  #convert string to digital
	dim_list.append(dim)
	word2vector[word] = dim
	line = file.readline()

pca = decomposition.PCA(n_components = 2)  #using PCA dim reduce
pca = pca.fit_transform(dim_list)
pylot.figure(1)
pylot.plot(pca[:, 0], pca[:, 1], 'bo')
pylot.show()

tsne = TSNE(learning_rate = 100).fit_transform(dim_list)
pylot.figure(2)
pylot.plot(tsne[:, 0], tsne[:, 1], 'ro')
pylot.show()
