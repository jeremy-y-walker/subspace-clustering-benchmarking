library("readmnist")
library('MixGHD')
library("cluster")
library("clustrd")

#####
setwd('/Users/Admin/Desktop/Stats Classes/MNIST/fashion')
mnist_fashion_Xtrain = matrix(Read.mnist("train-images-idx3-ubyte")$pic, nrow = 60000, byrow = TRUE)
mnist_fashion_Ytrain = Read.mnist("train-labels-idx1-ubyte")$labels
mnist_fashion_Xtest = matrix(Read.mnist("t10k-images-idx3-ubyte")$pic, nrow = 10000, byrow = TRUE)
mnist_fashion_Ytest = Read.mnist("t10k-labels-idx1-ubyte")$labels

mnist_fashion_X = rbind(mnist_fashion_Xtrain,mnist_fashion_Xtest)
mnist_fashion_Y = c(mnist_fashion_Ytrain,mnist_fashion_Ytest)

Index = c()
for(i in 0:9) {
  subsetIdx = which(mnist_fashion_Y == i)
  Sample = sample(subsetIdx, size = 200, replace = FALSE)
  Index = c(Index,Sample)
}
mnist_fashion_data = mnist_fashion_X[Index,]
mnist_fashion_labels = mnist_fashion_Y[Index]

trueLabel_f=mnist_fashion_labels + 1

KMscore1f = c(); FKMscore1f = c(); RKMscore1f = c()
km_ari_matrix1f = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix1f = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix1f = matrix(,nrow = 20, ncol = 5) 
# { #perform PCA on each simulated data matrix; find a dimension to reduce to
pca = princomp(x=mnist_fashion_data)
expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
s = max(which(round(expVar,2) >= 0.80)[1],2)
sim_pca = pca$scores[,1:s]; print("PCA done")
#######  

avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
for(g in 5:15) { #perform k-means after PCA
  kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
  partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabel_d)
  sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  kmARI = c(kmARI, ariVal)
}
KMscore1f = c(KMscore1f, which(avgSil == max(avgSil)) + 1)
km_ari_matrix1f = kmARI
########
# second pca
pca = princomp(x=sim_pca)
expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
s = max(which(round(expVar,2) >= 0.80)[1],2)
sim_pca = pca$scores[,1:s]; print("PCA done")


#########

avgSil = c(); fkmARI = c()
for(g in 5:15) { #perform FKM
  FKM_out = cluspca(data = mnist_fashion_data, nclus = g, ndim = s, method = "FKM")
  sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  ariVal = ARI(FKM_out$cluster, trueLabel_d); fkmARI = c(fkmARI, ariVal)
}
FKMscore1f = c(FKMscore1f, which(avgSil == max(avgSil)) + 1)
fkm_ari_matrix1f = fkmARI

avgSil = c(); rkmARI = c()
for(g in 5:15) { #perform RKM
  RKM_out = cluspca(data = mnist_fashion_data, nclus = g, ndim = s, method = "RKM")
  sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  ariVal = ARI(RKM_out$cluster, trueLabel_d); rkmARI = c(rkmARI, ariVal)
}
RKMscore1f = c(RKMscore1f, which(avgSil == max(avgSil)) + 1)
rkm_ari_matrix1f = rkmARI
print(i)




