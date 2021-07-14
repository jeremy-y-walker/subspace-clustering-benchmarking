library("kernlab")

#SIM DATA 1
SPECscore1 = c(); spec_ari_matrix1 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_1[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_1[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix1[i,] = specARI
  SPECscore1 = c(SPECscore1, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 2
SPECscore2 = c(); spec_ari_matrix2 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_2[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_2[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix2[i,] = specARI
  SPECscore2 = c(SPECscore2, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 3
SPECscore3 = c(); spec_ari_matrix3 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_3[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_3[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix3[i,] = specARI
  SPECscore3 = c(SPECscore3, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 4
SPECscore4 = c(); spec_ari_matrix4 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_4[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_4[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix4[i,] = specARI
  SPECscore4 = c(SPECscore4, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 5
SPECscore5 = c(); spec_ari_matrix5 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_5[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_5[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix5[i,] = specARI
  SPECscore5 = c(SPECscore5, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 6
SPECscore6 = c(); spec_ari_matrix6 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_6[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_6[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix6[i,] = specARI
  SPECscore6 = c(SPECscore6, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 7
SPECscore7 = c(); spec_ari_matrix7 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_7[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_7[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix7[i,] = specARI
  SPECscore7 = c(SPECscore7, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 8
SPECscore8 = c(); spec_ari_matrix8 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_8[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_8[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix8[i,] = specARI
  SPECscore8 = c(SPECscore8, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 9
SPECscore9 = c(); spec_ari_matrix9 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_9[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_9[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix9[i,] = specARI
  SPECscore9 = c(SPECscore9, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 10
SPECscore10 = c(); spec_ari_matrix10 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_10[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_10[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix10[i,] = specARI
  SPECscore10 = c(SPECscore10, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 11
SPECscore11 = c(); spec_ari_matrix11 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_11[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_11[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix11[i,] = specARI
  SPECscore11 = c(SPECscore11, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 12
SPECscore12 = c(); spec_ari_matrix12 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_12[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_12[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix12[i,] = specARI
  SPECscore12 = c(SPECscore12, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 13
SPECscore13 = c(); spec_ari_matrix13 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_13[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_13[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix13[i,] = specARI
  SPECscore13 = c(SPECscore13, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 14
SPECscore14 = c(); spec_ari_matrix14 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_14[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_14[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix14[i,] = specARI
  SPECscore14 = c(SPECscore14, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 15
SPECscore15 = c(); spec_ari_matrix15 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_15[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_15[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix15[i,] = specARI
  SPECscore15 = c(SPECscore15, which(avgSil == max(avgSil)) + 1)
  print(i)
}

#SIM DATA 16
SPECscore16 = c(); spec_ari_matrix16 = matrix(,nrow = 20, ncol = 5)
for(i in 1:20) {
  specARI = c(); distMatrix = dist(sim_tensor_16[,,i]); avgSil = c()
  for(g in 2:6) {
    specClust_out = specc(x = sim_tensor_16[,,i], centers = g)
    ariVal = ARI(trueLabels, specClust_out@.Data); specARI = c(specARI, ariVal)
    sil = silhouette(specClust_out@.Data, distMatrix); avgSil = c(avgSil, mean(sil[,3]))
  }
  spec_ari_matrix16[i,] = specARI
  SPECscore16 = c(SPECscore16, which(avgSil == max(avgSil)) + 1)
  print(i)
}


