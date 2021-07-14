#Read MNIST digits and fashion data
library("readmnist")
library('MixGHD')
library("cluster")
library("clustrd")

setwd('/Users/jeremywalker/Desktop/Math 253/Project')
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

setwd('/Users/jeremywalker/Desktop/Math 251/Data')
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

###### SIMULATED DATA ANALYSIS ######
trueLabels = c(rep(1,200),rep(2,200),rep(3,200),rep(4,200))

#SIM DATA 1
KMscore1 = c(); FKMscore1 = c(); RKMscore1 = c()
km_ari_matrix1 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix1 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix1 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_1[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore1 = c(KMscore1, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix1[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_1[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore1 = c(FKMscore1, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix1[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_1[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore1 = c(RKMscore1, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix1[i,] = rkmARI
  print(i)
}

#SIM DATA 2
KMscore2 = c(); FKMscore2 = c(); RKMscore2 = c()
km_ari_matrix2 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix2 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix2 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_2[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore2 = c(KMscore2, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix2[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_2[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore2 = c(FKMscore2, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix2[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_2[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore2 = c(RKMscore2, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix2[i,] = rkmARI
  print(i)
}

#SIM DATA 3
KMscore3 = c(); FKMscore3 = c(); RKMscore3 = c()
km_ari_matrix3 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix3 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix3 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_3[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore3 = c(KMscore3, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix3[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_3[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore3 = c(FKMscore3, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix3[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_3[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore3 = c(RKMscore3, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix3[i,] = rkmARI
  print(i)
}

#SIM DATA 4
KMscore4 = c(); FKMscore4 = c(); RKMscore4 = c()
km_ari_matrix4 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix4 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix4 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_4[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore4 = c(KMscore4, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix4[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_4[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore4 = c(FKMscore4, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix4[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_4[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore4 = c(RKMscore4, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix4[i,] = rkmARI
  print(i)
}

#SIM DATA 5
KMscore5 = c(); FKMscore5 = c(); RKMscore5 = c()
km_ari_matrix5 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix5 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix5 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_5[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore5 = c(KMscore5, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix5[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_5[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore5 = c(FKMscore5, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix5[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_5[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore5 = c(RKMscore5, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix5[i,] = rkmARI
  print(i)
}

#SIM DATA 6
KMscore6 = c(); FKMscore6 = c(); RKMscore6 = c()
km_ari_matrix6 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix6 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix6 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_6[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore6 = c(KMscore6, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix6[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_6[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore6 = c(FKMscore6, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix6[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_6[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore6 = c(RKMscore6, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix6[i,] = rkmARI
  print(i)
}

#SIM DATA 7
KMscore7 = c(); FKMscore7 = c(); RKMscore7 = c()
km_ari_matrix7 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix7 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix7 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_7[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore7 = c(KMscore7, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix7[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_7[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore7 = c(FKMscore7, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix7[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_7[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore7 = c(RKMscore7, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix7[i,] = rkmARI
  print(i)
}

#SIM DATA 8
KMscore8 = c(); FKMscore8 = c(); RKMscore8 = c()
km_ari_matrix8 = matrix(,nrow = 20, ncol = 5); fkm_ari_matrix8 = matrix(,nrow = 20, ncol = 5); rkm_ari_matrix8 = matrix(,nrow = 20, ncol = 5) 
for(i in 1:20) { #perform PCA on each simulated data matrix; find a dimension to reduce to
  pca = princomp(x=sim_tensor_8[,,i])
  expVar = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  s = max(which(round(expVar,2) >= 0.90)[1],2)
  sim_pca = pca$scores[,1:s]; print("PCA done")
  
  avgSil = c(); distMatrix = dist(sim_pca); kmARI = c()
  for(g in 2:6) { #perform k-means after PCA
    kmeans_out = kmeans(x = sim_pca, centers = g, iter.max = 500, nstart = 50)
    partition = kmeans_out$cluster; ariVal = ARI(partition, trueLabels)
    sil = silhouette(partition, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    kmARI = c(kmARI, ariVal)
  }
  KMscore8 = c(KMscore8, which(avgSil == max(avgSil)) + 1)
  km_ari_matrix8[i,] = kmARI
  
  avgSil = c(); fkmARI = c()
  for(g in 2:6) { #perform FKM
    FKM_out = cluspca(data = sim_tensor_8[,,i], nclus = g, ndim = s, method = "FKM")
    sil = silhouette(FKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(FKM_out$cluster, trueLabels); fkmARI = c(fkmARI, ariVal)
  }
  FKMscore8 = c(FKMscore8, which(avgSil == max(avgSil)) + 1)
  fkm_ari_matrix8[i,] = fkmARI
  
  avgSil = c(); rkmARI = c()
  for(g in 2:6) { #perform RKM
    RKM_out = cluspca(data = sim_tensor_8[,,i], nclus = g, ndim = s, method = "RKM")
    sil = silhouette(RKM_out$cluster, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
    ariVal = ARI(RKM_out$cluster, trueLabels); rkmARI = c(rkmARI, ariVal)
  }
  RKMscore8 = c(RKMscore8, which(avgSil == max(avgSil)) + 1)
  rkm_ari_matrix8[i,] = rkmARI
  print(i)
}

###### MNIST DATA ANALYSIS ######
mnist_digits_pca = princomp(x = mnist_digits_data)
plot(cumsum(mnist_digits_pca$sdev^2 / sum(mnist_digits_pca$sdev^2))) #142 is 95%
mnist_digits_pca95 = mnist_digits_pca$scores[,1:142]
plot(mnist_digits_pca95[,1:2], col = as.factor(mnist_digits_labels), cex = 0.7, pch = 16,
     xlab = "Principal Component 1", ylab = "Principal Component 2", main = "MNIST Digits")

mnist_fashion_pca = princomp(x = mnist_fashion_data)
plot(cumsum(mnist_fashion_pca$sdev^2 / sum(mnist_fashion_pca$sdev^2))) #160 is 95%
mnist_fashion_pca95 = mnist_fashion_pca$scores[,1:160]
plot(mnist_fashion_pca95[,1:2], col = as.factor(mnist_fashion_labels), cex = 0.7, pch = 16,
     xlab = "Principal Component 1", ylab = "Principal Component 2", main = "MNIST Fashion")

trueLabels = c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),rep(5,200),rep(6,200),rep(7,200),rep(8,200),rep(9,200),rep(10,200))

# DIGITS #
distMatrix = dist(mnist_digits_pca95)
digits_km_avgSil = c(); digits_kmARI = c()
for(g in 5:15) {
  kmeans_out = kmeans(x = mnist_digits_pca95, centers = g, iter.max = 500, nstart = 50)
  sil = silhouette(kmeans_out$cluster, distMatrix); digits_km_avgSil = c(digits_km_avgSil, mean(sil[,3]))
  ariVal = ARI(kmeans_out$cluster, trueLabels); digits_kmARI = c(digits_kmARI, ariVal)
  print(g)
}

cumsum(princomp(mnist_digits_pca95)$sdev^2 / sum(princomp(mnist_digits_pca95)$sdev^2)) #85 PCs
digits_fkm_avgSil = c(); digits_fkmARI = c()
for(g in 5:15) {
  FKM_out = cluspca(data = mnist_digits_pca95, nclus = g, ndim = 85, method = "FKM")
  sil = silhouette(FKM_out$cluster, distMatrix); digits_fkm_avgSil = c(digits_fkm_avgSil, mean(sil[,3]))
  ariVal = ARI(FKM_out$cluster, trueLabels); digits_fkmARI = c(digits_fkmARI, ariVal)
  print(g)
}

digits_rkm_avgSil = c(); digits_rkmARI = c()
for(g in 5:15) {
  RKM_out = cluspca(data = mnist_digits_pca95, nclus = g, ndim = 85, method = "RKM")
  sil = silhouette(RKM_out$cluster, distMatrix); digits_rkm_avgSil = c(digits_rkm_avgSil, mean(sil[,3]))
  ariVal = ARI(RKM_out$cluster, trueLabels); digits_rkmARI = c(digits_rkmARI, ariVal)
  print(g)
}

digits_spec_avgSil = c(); digits_specARI = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_digits_pca95, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); digits_spec_avgSil = c(digits_spec_avgSil, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); digits_specARI = c(digits_specARI, ariVal)
  print(g)
}

# FASHION #
distMatrix = dist(mnist_fashion_pca95)
fashion_km_avgSil = c(); fashion_kmARI = c()
for(g in 5:15) {
  kmeans_out = kmeans(x = mnist_fashion_pca95, centers = g, iter.max = 500, nstart = 50)
  sil = silhouette(kmeans_out$cluster, distMatrix); fashion_km_avgSil = c(fashion_km_avgSil, mean(sil[,3]))
  ariVal = ARI(kmeans_out$cluster, trueLabels); fashion_kmARI = c(fashion_kmARI, ariVal)
  print(g)
}

cumsum(princomp(mnist_fashion_pca95)$sdev^2 / sum(princomp(mnist_fashion_pca95)$sdev^2))
fashion_fkm_avgSil = c(); fashion_fkmARI = c()
for(g in 5:15) {
  FKM_out = cluspca(data = mnist_fashion_pca95, nclus = g, ndim = 80, method = "FKM")
  sil = silhouette(FKM_out$cluster, distMatrix); fashion_fkm_avgSil = c(fashion_fkm_avgSil, mean(sil[,3]))
  ariVal = ARI(FKM_out$cluster, trueLabels); fashion_fkmARI = c(fashion_fkmARI, ariVal)
  print(g)
}

fashion_rkm_avgSil = c(); fashion_rkmARI = c()
for(g in 5:15) {
  RKM_out = cluspca(data = mnist_fashion_pca95, nclus = g, ndim = 80, method = "RKM")
  sil = silhouette(RKM_out$cluster, distMatrix); fashion_rkm_avgSil = c(fashion_rkm_avgSil, mean(sil[,3]))
  ariVal = ARI(RKM_out$cluster, trueLabels); fashion_rkmARI = c(fashion_rkmARI, ariVal)
  print(g)
}

fashion_spec_avgSil = c(); fashion_specARI = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_fashion_pca95, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); fashion_spec_avgSil = c(fashion_spec_avgSil, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); fashion_specARI = c(fashion_specARI, ariVal)
  print(g)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sum(KMscore1==4)/20;getmode(KMscore1)
sum(KMscore2==4)/20;getmode(KMscore2)
sum(KMscore3==4)/20;getmode(KMscore3)
sum(KMscore4==4)/20;getmode(KMscore4)
sum(KMscore5==4)/20;getmode(KMscore5)
sum(KMscore6==4)/20;getmode(KMscore6)
sum(KMscore7==4)/20;getmode(KMscore7)
sum(KMscore8==4)/20;getmode(KMscore8)

sum(FKMscore1==4)/20;getmode(FKMscore1)
sum(FKMscore2==4)/20;getmode(FKMscore2)
sum(FKMscore3==4)/20;getmode(FKMscore3)
sum(FKMscore4==4)/20;getmode(FKMscore4)
sum(FKMscore5==4)/20;getmode(FKMscore5)
sum(FKMscore6==4)/20;getmode(FKMscore6)
sum(FKMscore7==4)/20;getmode(FKMscore7)
sum(FKMscore8==4)/20;getmode(FKMscore8)

sum(RKMscore1==4)/20;getmode(RKMscore1)
sum(RKMscore2==4)/20;getmode(RKMscore2)
sum(RKMscore3==4)/20;getmode(RKMscore3)
sum(RKMscore4==4)/20;getmode(RKMscore4)
sum(RKMscore5==4)/20;getmode(RKMscore5)
sum(RKMscore6==4)/20;getmode(RKMscore6)
sum(RKMscore7==4)/20;getmode(RKMscore7)
sum(RKMscore8==4)/20;getmode(RKMscore8)

sum(SPECscore1==4)/20;getmode(SPECscore1)
sum(SPECscore2==4)/20;getmode(SPECscore2)
sum(SPECscore3==4)/20;getmode(SPECscore3)
sum(SPECscore4==4)/20;getmode(SPECscore4)
sum(SPECscore5==4)/20;getmode(SPECscore5)
sum(SPECscore6==4)/20;getmode(SPECscore6)
sum(SPECscore7==4)/20;getmode(SPECscore7)
sum(SPECscore8==4)/20;getmode(SPECscore8)
sum(SPECscore9==4)/20;getmode(SPECscore9)
sum(SPECscore10==4)/20;getmode(SPECscore10)
sum(SPECscore11==4)/20;getmode(SPECscore11)
sum(SPECscore12==4)/20;getmode(SPECscore12)
sum(SPECscore13==4)/20;getmode(SPECscore13)
sum(SPECscore14==4)/20;getmode(SPECscore14)
sum(SPECscore15==4)/20;getmode(SPECscore15)
sum(SPECscore16==4)/20;getmode(SPECscore16)

colMeans(km_ari_matrix1)[3];sqrt(var(km_ari_matrix1[,3]))
colMeans(km_ari_matrix2)[3];sqrt(var(km_ari_matrix2[,3]))
colMeans(km_ari_matrix3)[3];sqrt(var(km_ari_matrix3[,3]))
colMeans(km_ari_matrix4)[3];sqrt(var(km_ari_matrix4[,3]))
colMeans(km_ari_matrix5)[3];sqrt(var(km_ari_matrix5[,3]))
colMeans(km_ari_matrix6)[3];sqrt(var(km_ari_matrix6[,3]))
colMeans(km_ari_matrix7)[3];sqrt(var(km_ari_matrix7[,3]))
colMeans(km_ari_matrix8)[3];sqrt(var(km_ari_matrix8[,3]))

colMeans(fkm_ari_matrix1)[3];sqrt(var(fkm_ari_matrix1[,3]))
colMeans(fkm_ari_matrix2)[3];sqrt(var(fkm_ari_matrix2[,3]))
colMeans(fkm_ari_matrix3)[3];sqrt(var(fkm_ari_matrix3[,3]))
colMeans(fkm_ari_matrix4)[3];sqrt(var(fkm_ari_matrix4[,3]))
colMeans(fkm_ari_matrix5)[3];sqrt(var(fkm_ari_matrix5[,3]))
colMeans(fkm_ari_matrix6)[3];sqrt(var(fkm_ari_matrix6[,3]))
colMeans(fkm_ari_matrix7)[3];sqrt(var(fkm_ari_matrix7[,3]))
colMeans(fkm_ari_matrix8)[3];sqrt(var(fkm_ari_matrix8[,3]))

colMeans(rkm_ari_matrix1)[3];sqrt(var(rkm_ari_matrix1[,3]))
colMeans(rkm_ari_matrix2)[3];sqrt(var(rkm_ari_matrix2[,3]))
colMeans(rkm_ari_matrix3)[3];sqrt(var(rkm_ari_matrix3[,3]))
colMeans(rkm_ari_matrix4)[3];sqrt(var(rkm_ari_matrix4[,3]))
colMeans(rkm_ari_matrix5)[3];sqrt(var(rkm_ari_matrix5[,3]))
colMeans(rkm_ari_matrix6)[3];sqrt(var(rkm_ari_matrix6[,3]))
colMeans(rkm_ari_matrix7)[3];sqrt(var(rkm_ari_matrix7[,3]))
colMeans(rkm_ari_matrix8)[3];sqrt(var(rkm_ari_matrix8[,3]))

colMeans(spec_ari_matrix1)[3];sqrt(var(spec_ari_matrix1[,3]))
colMeans(spec_ari_matrix2)[3];sqrt(var(spec_ari_matrix2[,3]))
colMeans(spec_ari_matrix3)[3];sqrt(var(spec_ari_matrix3[,3]))
colMeans(spec_ari_matrix4)[3];sqrt(var(spec_ari_matrix4[,3]))
colMeans(spec_ari_matrix5)[3];sqrt(var(spec_ari_matrix5[,3]))
colMeans(spec_ari_matrix6)[3];sqrt(var(spec_ari_matrix6[,3]))
colMeans(spec_ari_matrix7)[3];sqrt(var(spec_ari_matrix7[,3]))
colMeans(spec_ari_matrix8)[3];sqrt(var(spec_ari_matrix8[,3]))
colMeans(spec_ari_matrix9)[3];sqrt(var(spec_ari_matrix9[,3]))
colMeans(spec_ari_matrix10)[3];sqrt(var(spec_ari_matrix10[,3]))
colMeans(spec_ari_matrix11)[3];sqrt(var(spec_ari_matrix11[,3]))
colMeans(spec_ari_matrix12)[3];sqrt(var(spec_ari_matrix12[,3]))
colMeans(spec_ari_matrix13)[3];sqrt(var(spec_ari_matrix13[,3]))
colMeans(spec_ari_matrix14)[3];sqrt(var(spec_ari_matrix14[,3]))
colMeans(spec_ari_matrix15)[3];sqrt(var(spec_ari_matrix15[,3]))
colMeans(spec_ari_matrix16)[3];sqrt(var(spec_ari_matrix16[,3]))


distMatrix = dist(mnist_digits_data)
digits_spec_avgSil_rerun = c(); digits_specARI_rerun = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_digits_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); digits_spec_avgSil_rerun = c(digits_spec_avgSil_rerun, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); digits_specARI_rerun = c(digits_specARI_rerun, ariVal)
  print(g)
}

distMatrix = dist(mnist_fashion_data)
fashion_spec_avgSil_rerun = c(); fashion_specARI_rerun = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_fashion_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); fashion_spec_avgSil_rerun = c(fashion_spec_avgSil_rerun, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); fashion_specARI_rerun = c(fashion_specARI_rerun, ariVal)
  print(g)
}

distMatrix = dist(mnist_digits_data)
digits_spec_avgSil_rerun2 = c(); digits_specARI_rerun2 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_digits_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); digits_spec_avgSil_rerun2 = c(digits_spec_avgSil_rerun2, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); digits_specARI_rerun2 = c(digits_specARI_rerun2, ariVal)
  print(g)
}

distMatrix = dist(mnist_fashion_data)
fashion_spec_avgSil_rerun2 = c(); fashion_specARI_rerun2 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_fashion_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); fashion_spec_avgSil_rerun2 = c(fashion_spec_avgSil_rerun2, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); fashion_specARI_rerun2 = c(fashion_specARI_rerun2, ariVal)
  print(g)
}

distMatrix = dist(mnist_digits_data)
digits_spec_avgSil_rerun3 = c(); digits_specARI_rerun3 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_digits_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); digits_spec_avgSil_rerun3 = c(digits_spec_avgSil_rerun3, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); digits_specARI_rerun3 = c(digits_specARI_rerun3, ariVal)
  print(g)
}

distMatrix = dist(mnist_fashion_data)
fashion_spec_avgSil_rerun3 = c(); fashion_specARI_rerun3 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_fashion_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); fashion_spec_avgSil_rerun3 = c(fashion_spec_avgSil_rerun3, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); fashion_specARI_rerun3 = c(fashion_specARI_rerun3, ariVal)
  print(g)
}

distMatrix = dist(mnist_digits_data)
digits_spec_avgSil_rerun4 = c(); digits_specARI_rerun4 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_digits_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); digits_spec_avgSil_rerun4 = c(digits_spec_avgSil_rerun4, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); digits_specARI_rerun4 = c(digits_specARI_rerun4, ariVal)
  print(g)
}

distMatrix = dist(mnist_fashion_data)
fashion_spec_avgSil_rerun4 = c(); fashion_specARI_rerun4 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_fashion_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); fashion_spec_avgSil_rerun4 = c(fashion_spec_avgSil_rerun4, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); fashion_specARI_rerun4 = c(fashion_specARI_rerun4, ariVal)
  print(g)
}

distMatrix = dist(mnist_digits_data)
digits_spec_avgSil_rerun5 = c(); digits_specARI_rerun5 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_digits_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); digits_spec_avgSil_rerun5 = c(digits_spec_avgSil_rerun5, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); digits_specARI_rerun5 = c(digits_specARI_rerun5, ariVal)
  print(g)
}

distMatrix = dist(mnist_fashion_data)
fashion_spec_avgSil_rerun5 = c(); fashion_specARI_rerun5 = c()
for(g in 5:15) {
  spec_out = specc(x = mnist_fashion_data, centers = g)
  sil = silhouette(spec_out@.Data, distMatrix); fashion_spec_avgSil_rerun5 = c(fashion_spec_avgSil_rerun5, mean(sil[,3]))
  ariVal = ARI(spec_out@.Data, trueLabels); fashion_specARI_rerun5 = c(fashion_specARI_rerun5, ariVal)
  print(g)
}

which(digits_spec_avgSil_rerun==max(digits_spec_avgSil_rerun))
which(digits_spec_avgSil_rerun2==max(digits_spec_avgSil_rerun2))
which(digits_spec_avgSil_rerun3==max(digits_spec_avgSil_rerun3))
which(digits_spec_avgSil_rerun4==max(digits_spec_avgSil_rerun4))
which(digits_spec_avgSil_rerun5==max(digits_spec_avgSil_rerun5))

which(fashion_spec_avgSil_rerun==max(fashion_spec_avgSil_rerun))
which(fashion_spec_avgSil_rerun2==max(fashion_spec_avgSil_rerun2))
which(fashion_spec_avgSil_rerun3==max(fashion_spec_avgSil_rerun3))
which(fashion_spec_avgSil_rerun4==max(fashion_spec_avgSil_rerun4))
which(fashion_spec_avgSil_rerun5==max(fashion_spec_avgSil_rerun5))

colMeans(rbind(digits_specARI_rerun,digits_specARI_rerun2,digits_specARI_rerun3,digits_specARI_rerun4,digits_specARI_rerun5))
sqrt(var(rbind(digits_specARI_rerun,digits_specARI_rerun2,digits_specARI_rerun3)[,6]))

colMeans(rbind(fashion_specARI_rerun,fashion_specARI_rerun2,fashion_specARI_rerun3,fashion_specARI_rerun4,fashion_specARI_rerun5))
sqrt(var(rbind(fashion_specARI_rerun,fashion_specARI_rerun2,fashion_specARI_rerun3)[,6]))
