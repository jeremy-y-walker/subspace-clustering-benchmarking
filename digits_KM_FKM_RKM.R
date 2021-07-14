library("readmnist")
library('MixGHD')
library("cluster")
library("clustrd")

setwd('/Users/MWY/Desktop/mnist')
mnist_digits_Xtrain = matrix(Read.mnist("train-images-idx3-ubyte")$pic, nrow = 60000, byrow = TRUE)
mnist_digits_Ytrain = Read.mnist("train-labels-idx1-ubyte")$labels
mnist_digits_Xtest = matrix(Read.mnist("t10k-images-idx3-ubyte")$pic, nrow = 10000, byrow = TRUE)
mnist_digits_Ytest = Read.mnist("t10k-labels-idx1-ubyte")$labels

mnist_digits_X = rbind(mnist_digits_Xtrain,mnist_digits_Xtest)
mnist_digits_Y = c(mnist_digits_Ytrain,mnist_digits_Ytest)

Index = c()
for(i in 0:9) {
  subsetIdx = which(mnist_digits_Y == i)
  Sample = sample(subsetIdx, size = 200, replace = FALSE)
  Index = c(Index,Sample)
}
mnist_digits_data = mnist_digits_X[Index,]
mnist_digits_labels = mnist_digits_Y[Index]

trueLabel_d=mnist_digits_labels

######

KMscore1 = c(); FKMscore1 = c(); RKMscore1 = c()
km_ari_matrix1 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix1 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix1 = matrix(,nrow = 20, ncol = 5) 
# { #perform PCA on each simulated data matrix; find a dimension to reduce to
pca = princomp(x=mnist_digits_data)
expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
s = max(which(round(expVar,2) >= 0.95)[1],2)
sim_pca = pca$scores[,1:s]; print("PCA done")
#######  
  
avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
for(g in 5:15) { #perform k-means after PCA
  kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
  partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabel_d)
  sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  kmARI = c(kmARI, ariVal)
}
KMscore1 = c(KMscore1, which(avgSil == max(avgSil)) + 1)
km_ari_matrix1 = kmARI
########
  
#2st pca
pca = princomp(x=sim_pca)
expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
s = max(which(round(expVar,2) >= 0.95)[1],2)
sim_pca = pca$scores[,1:s]; print("PCA done")
  

######## FKM & RKM    
  
avgSil = c(); fkmARI = c()
for(g in 5:15) { #perform FKM
  FKM_out = cluspca(data = sim_pca, nclus = g, ndim = s, method = "FKM")
  sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  ariVal = ARI(FKM_out$cluster, trueLabel_d); fkmARI = c(fkmARI, ariVal)
}
FKMscore1 = c(FKMscore1, which(avgSil == max(avgSil)) + 1)
fkm_ari_matrix1 = fkmARI
  
avgSil = c(); rkmARI = c()
for(g in 5:15) { #perform RKM
  RKM_out = cluspca(data = sim_pca, nclus = g, ndim = s, method = "RKM")
  sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  ariVal = ARI(RKM_out$cluster, trueLabel_d); rkmARI = c(rkmARI, ariVal)
}
RKMscore1 = c(RKMscore1, which(avgSil == max(avgSil)) + 1)
rkm_ari_matrix1 = rkmARI



  


