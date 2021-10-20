library(readxl)
library(MESS)
library(MASS)
library(forecast)
library(tidyverse)

X <- read_excel("C:/Users/John/Desktop/STAT 445/Data/malifarmdata.xlsx")
X
# a
# i
pairs(X, diag.panel = panel.hist)

par(mfcol=c(9, 9), mar=c(2,2,2,2))
for(i in 1:9){
  for(j in 1:9) {
    if(i != j){
      plot(X[[i]],X[[j]],type="n", xlab = paste0("X", i), ylab = paste0("X", j))
      text(X[[i]],X[[j]], cex = 0.8) 
      }
    else{
      hist(X[[i]], xlab = paste0("X", i), main = paste0("Histogram of X", i), prob=TRUE,cex.main=0.8)
      lines(density(X[[i]]))}
  }
}
X
# ii
par(mfrow=c(1, 1))
boxplot(X)
# iii
S <- cov(X)
x_bar <- colMeans(X)
D <- c() 
for (i in 1:nrow(X)) {
  x_i <- t(X[i,])
  D[i]=t(x_i-x_bar)%*%solve(S)%*%(x_i-x_bar)
}
round(D,3)
par(mfrow=c(1, 1))
plot(D, main = "D values vs index", type="n");text(D)
which(D>20)

par(mfrow=c(3, 3))
q <- vector("list", length = 9)
for (i in 1:9) {
  q[[i]]=qqnorm(X[[i]],type="n", main = paste0("Normal Q-Q Plot of X",i));qqline(X[[i]],col="red")
  text(q[[i]]$x, q[[i]]$y,  pos = 1, cex=1.5)
}


dtable <- data.frame(matrix(ncol = 9, nrow = 3))
colnames(dtable) <- colnames(X)
rownames(dtable) <- c("Scatter Plots", "Q-Q Plots", "Distance Plot")
dtable
# 25 34 69 72 remove
X
Y <- X[-c(25,34,52,57,62,69,72),]
pairs(Y, diag.panel = panel.hist)

# b
# PCA on X
# i
S <- cov(X)
S
# ii
spectral_decomp_X <- eigen(S,symmetric=T,only.values=F)
eigenvalues_X <- spectral_decomp_X$values
eigenvectors_X <- spectral_decomp_X$vectors
# the first contributes to the majority of the variance, 
# 2nd has significant contribution as well
# 4th to 9th small, there is some linear relationship between some of the experiments

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
# suggest keeping one, 89.5%

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_X>mean(eigenvalues_X))
# also sugges keeping one

# criteria 3, screeplot, look for a "bend" in the plot 
screeplot(prcomp(X) ,type="line")
# bend hapens at, suggesting keep one
# all three suggest keeping 1, but we keey 2 here just to do the PCA
view(X)
# iv
eigenvectors_X[,1]
eigenvectors_X[,2]

# v
# PC1, mostly depends on 2nd variable DistRD, looks like PC1 takes about locations
# PC2, mostly depends on 1st variable Family, PC2 is about family
# PC2, some dependencies on cattle, indicates milk? as most family drink milk most than other farn products?

# vi
pc_X1 <- as.matrix(X)%*%eigenvectors_X[,1]
pc_X2 <- as.matrix(X)%*%eigenvectors_X[,2]
plot(pc_X1,pc_X2)
# most of the components has high values on PC1
# this means DistRD plays a big role here

# PCA on Y
# i
Sy <- cov(Y)
Sy

# ii
spectral_decomp_Y <- eigen(Sy,symmetric=T,only.values=F)
eigenvalues_Y <- spectral_decomp_Y$values
eigenvectors_Y <- spectral_decomp_Y$vectors

eigenvalues_Y
# the first AND second PC contributes to the majority of the variance, 
# the rest are small, showing dependencies

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
# sugeest keeping 2 PCs, gives us 94.9%

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_Y>mean(eigenvalues_Y))
# this method suggest just keep 2

# criteria 3, screeplot, look for a "bend" in the plot 
screeplot(prcomp(Y) ,type="line")
# bend happens at the 3rd variable, suggest keeping 2
# over all, keeps two , very nice, i like it

# iv
eigenvectors_Y[,1]
eigenvectors_Y[,2]

# v
# PC1, similar to before, still mostly depends on 2nd variable DistRD, this PC is mostly a interpretation of DistRD
# PC2, similar to before, mostly depends on Family, some dependencies on cattle, PC2 is mostly about family

# vi
pc_Y1 <- as.matrix(Y)%*%eigenvectors_Y[,1]
pc_Y2 <- as.matrix(Y)%*%eigenvectors_Y[,2]
plot(pc_Y1,pc_Y2)
# most of the components has medium to high values on PC1
# DistRD still plays a big role here

# C
# First, the outliers cause some scale issues. As in the first PC scatter plot things are scatters.
# Also, it made PC2 way less significant, variace conribution only shows 8.1%, but in the seond one after removal
# it's 39.5%. This indicats that PC2 got overshadowed by PC1 and becomes less significant.

# The result after outlier removal is better. Because in the first one, some information got hidden/overshadowed.
# Also scaling on the graph for the second one is better.
