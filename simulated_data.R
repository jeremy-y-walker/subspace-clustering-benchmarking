#Simulated Data
library("Matrix")

#Generate variance matrices
largeVar = diag(runif(200,7,10),nrow=200); smallVar = diag(runif(200,0,2),nrow=200)
positiveCorr = matrix(runif(200^2,1,3.9),nrow=200); diag(positiveCorr) = runif(200,4,6); positiveCorr = as.matrix(forceSymmetric(positiveCorr))
negativeCorr = matrix(runif(200^2,-3.9,-1),nrow=200); diag(negativeCorr) = runif(200,4,6); negativeCorr = as.matrix(forceSymmetric(negativeCorr))

#Generate skew vectors
small = runif(200,-0.25,0.25); large = runif(200,-2,2)

sim_tensor_1 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  sim_tensor_1[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_2 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  sim_tensor_2[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_3 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  sim_tensor_3[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_4 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  sim_tensor_4[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_5 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  sim_tensor_5[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_6 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  sim_tensor_6[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_7 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  sim_tensor_7[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_8 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-10,10),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  sim_tensor_8[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_9 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=small,omega=1,lambda=.5)
  sim_tensor_9[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_10 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=largeVar,alpha=large,omega=1,lambda=.5)
  sim_tensor_10[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_11 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=small,omega=1,lambda=.5)
  sim_tensor_11[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_12 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=smallVar,alpha=large,omega=1,lambda=.5)
  sim_tensor_12[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_13 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=small,omega=1,lambda=.5)
  sim_tensor_13[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_14 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=negativeCorr,alpha=large,omega=1,lambda=.5)
  sim_tensor_14[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_15 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=small,omega=1,lambda=.5)
  sim_tensor_15[,,i]=rbind(clust1,clust2,clust3,clust4)
}

sim_tensor_16 = array(dim = c(800,200,20))
for(i in 1:20) {
  clust1=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  clust2=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  clust3=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  clust4=rGHD(200,p=200,mu=runif(200,-25,25),sigma=positiveCorr,alpha=large,omega=1,lambda=.5)
  sim_tensor_16[,,i]=rbind(clust1,clust2,clust3,clust4)
}

par(mfrow=c(2,2))
plot(sim_tensor_1[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 1",pch=16,cex=0.7)
plot(sim_tensor_2[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 2",pch=16,cex=0.7)
plot(sim_tensor_3[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 3",pch=16,cex=0.7)
plot(sim_tensor_4[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 4",pch=16,cex=0.7)

par(mfrow=c(2,2))
plot(sim_tensor_5[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 5",pch=16,cex=0.7)
plot(sim_tensor_6[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 6",pch=16,cex=0.7)
plot(sim_tensor_7[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 7",pch=16,cex=0.7)
plot(sim_tensor_8[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 8",pch=16,cex=0.7)

par(mfrow=c(2,2))
plot(sim_tensor_9[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 9",pch=16,cex=0.7)
plot(sim_tensor_10[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 10",pch=16,cex=0.7)
plot(sim_tensor_11[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 11",pch=16,cex=0.7)
plot(sim_tensor_12[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 12",pch=16,cex=0.7)

par(mfrow=c(2,2))
plot(sim_tensor_13[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 13",pch=16,cex=0.7)
plot(sim_tensor_14[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 14",pch=16,cex=0.7)
plot(sim_tensor_15[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 15",pch=16,cex=0.7)
plot(sim_tensor_16[,,1][,1:2],col=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)),
     xlab="Dimension 1", ylab = "Dimension 2", main = "Simulation Condition 16",pch=16,cex=0.7)


