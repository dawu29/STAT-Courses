library(readxl)
library(MESS)
library(MASS)
library(forecast)
library(tidyverse)

data <- read_excel("C:/Users/John/Desktop/STAT 445/Data/fleabeetledata.xlsx")
data <- data[-(1:2),-1]
sprintf("x%d",2)
data <- sapply(data, as.numeric)
data

# a
# Analysis on group 1/species 1
X <- data[-20,1:4]
X
# i
Sx <- cov(X)
Sx

# ii
spectral_decomp_X <- eigen(Sx,symmetric=T,only.values=F)
eigenvalues_X <- spectral_decomp_X$values
eigenvectors_X <- spectral_decomp_X$vectors

eigenvalues_X 
# the first contributes to the majority of the variance, 
# 2nd has significant contribution as well
# 3rd and 4th are small, there is some linear relationship between some of the experiments

# iii
# criteria 1, eigenvalues cumulative proportion table
total_var_X <- sum(eigenvalues_X)
prop_var_X <- eigenvalues_X/total_var_X
prop_var_X
cum_var_X <- c()
for (i in seq_along(prop_var_X)){
  cum_var_X <- c(cum_var_X,sum(prop_var_X[c(1:i)]))
}
tableX <- round(cbind(eigenvalues_X,prop_var_X,cum_var_X),3)
colnames(tableX) <- c("Eigenvalue","Prop. of variance", "Cumul.prop.")
tableX
# keeping three components gives us 87.1% of the variance, suggesting keep 2

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_X>mean(eigenvalues_X))
# this method suggest just keep 1

# criteria 3, screeplot, look for a "bend" in the plot 
screeplot(prcomp(X) ,type="line")
# bend happens has the 3rd one, suggest keeping 2
# overall,we keep 2 principle components

# iv
eigenvectors_X[,1]
eigenvectors_X[,2]

# v
# the 1st PC depends on the 2nd variable the most, followed by 1st and 4th variables.
# the 2nd PC depends on the 4th variable the most, followed by 2nd. 2nd and 3rd are in the opposite direction of the 4th
# 3rd doesnt contribute much, shows some linear dependencies
X
# vi
pc_X1 <- as.matrix(X)%*%eigenvectors_X[,1]
pc_X2 <- as.matrix(X)%*%eigenvectors_X[,2]
pcm_X <- cbind(pc_X1, pc_X2)
pcm_X
plot(pcm_X)
# interesting,  not much linear relationship there

# Analysis on group 2/species 2
Y <- data[,5:8]
Y
# i
Sy <- cov(Y)
Sy

# ii
spectral_decomp_Y <- eigen(Sy,symmetric=T,only.values=F)
eigenvalues_Y <- spectral_decomp_Y$values
eigenvectors_Y <- spectral_decomp_Y$vectors

eigenvalues_Y
# the first contributes to the majority of the variance, 
# 2nd has significant contribution as well
# 3rd and 4th are small, there is some linear relationship between some of the experiments

# iii
# criteria 1, eigenvalues cumulative proportion table
total_var_Y <- sum(eigenvalues_Y)
prop_var_Y <- eigenvalues_Y/total_var_Y
prop_var_Y
cum_var_Y <- c()
for (i in seq_along(prop_var_Y)){
  cum_var_Y <- c(cum_var_Y,sum(prop_var_Y[c(1:i)]))
}
tableY <- round(cbind(eigenvalues_Y,prop_var_Y,cum_var_Y),3)
colnames(tableY) <- c("Eigenvalue","Prop. of variance", "Cumul.prop.")
tableY
# keeping two components gives us 83.8% of the variance, suggesting keep 2

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_Y>mean(eigenvalues_Y))
# this method suggest just keep 1

# criteria 3, screeplot, look for a "bend" in the plot 
screeplot(prcomp(Y) ,type="line")
# bend happens has the 2nd one, suggest keeping 1
# overall,we keep 2 principle components so we can do scatter plot on PC1 and PC2

# iv
eigenvectors_Y[,1]
eigenvectors_Y[,2]

# V
# 1st PC depends on the 2nd variable the most, the about the same among the other 3
# 2nd PC depends on the 4th variable the most, the about the sam among the other 3

# vi
pc_Y1 <- as.matrix(Y)%*%eigenvectors_Y[,1]
pc_Y2 <- as.matrix(Y)%*%eigenvectors_Y[,2]
pcm_Y <- cbind(pc_Y1, pc_Y2)
pcm_Y
plot(pcm_Y)

# Analysis using both groups
D <-rbind(X,Y)

# i
Sd <- cov(D)
Sd

# ii
spectral_decomp_D <- eigen(Sd,symmetric=T,only.values=F)
eigenvalues_D <- spectral_decomp_D$values
eigenvectors_D <- spectral_decomp_D$vectors

eigenvalues_D
# the first contributes to the majority of the variance

# iii
# criteria 1, eigenvalues cumulative proportion table
total_var_D <- sum(eigenvalues_D)
prop_var_D <- eigenvalues_D/total_var_D
prop_var_D
cum_var_D <- c()
for (i in seq_along(prop_var_D)){
  cum_var_D <- c(cum_var_D,sum(prop_var_D[c(1:i)]))
}
tableD <- round(cbind(eigenvalues_D,prop_var_D,cum_var_D),3)
colnames(tableD) <- c("Eigenvalue","Prop. of variance", "Cumul.prop.")
tableD
# keeping three components gives us 83.9% of the variance, suggesting keep 2

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_D>mean(eigenvalues_D))
# this method suggest just keep 1

# criteria 3, screeplot, look for a "bend" in the plot 
screeplot(prcomp(D) ,type="line")
# bend happens has the 2nd one, suggest keeping 1
# overall,we keep 2 principle components so we can do scatter plot on PC1 and PC2

# iv
eigenvectors_D[,1]
eigenvectors_D[,2]

# v
# PC1, 1st has little to non contribution, 2nd has the most, 3rd and 4th both significant contribution
# PC2, 1st has the most contribution, 2nd,3rd, and 4th has some contribution
# this is showing the 1st variable doesn't really depends on the other and has a different relationship

# vi

pc_D1 <- as.matrix(D)%*%eigenvectors_D[,1]
pc_D2 <- as.matrix(D)%*%eigenvectors_D[,2]
pcm_D <- cbind(pc_D1, pc_D2)
pcm_D
D <- cbind(D,c(1))
D[19:37,5] <- 2
D
plot(pcm_D, pch=D[,5])
legend("bottomright", legend=c("Haltica oleracea", "Haltica carduorum"), pch=c(1, 2),cex=0.7)

plot(pcm_X)
plot(pcm_Y)


# this shows clusters of two groups. group 1 is more scatter around, group two is more clustered in an area

par(mfrow=c(2, 2),mar=c(2,2,2,2))
plot(pcm_D, pch=D[,5])
legend("topright", legend=c("Haltica oleracea", "Haltica carduorum"), pch=c(1, 2),cex=0.5)
plot.new()
plot(pcm_X,pch=1)
plot(pcm_Y,pch=2)

# b 
# base on the scatter plot, the last one looks the the first one and the second one lying on top of each other
# not much changes b



