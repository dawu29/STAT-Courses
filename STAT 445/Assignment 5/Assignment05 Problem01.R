library(readxl)
library(MESS)
library(MASS)
library(forecast)

X <- read_excel("C:/Users/John/Desktop/STAT 445/Data/temperaturedata-clean.xlsx")
X
x_bar <- colMeans(X)
x_bar
Xn <-matrix()

# a
S <- cov(X)
R <- cor(X)
boxplot(X) #scaling issue

# subtract each column by col mean vector
X <- sweep(X,2,x_bar)

# b and c
# Analysis using covariance matrix S
spectral_decomp_S <- eigen(S,symmetric=T,only.values=F)
spectral_decomp_S
eigenvalues_S <- spectral_decomp_S$values
eigenvectors_S <- spectral_decomp_S$vectors
View(S)
# i

round(eigenvalues_S, 5)
total_var_S <- sum(eigenvalues_S)
# the total variance is 24381
# the first contributes to the majority of the variance,22303/24381

# ii
# criteria 1, eigenvalues cumulative proportion table
prop_var_S <- eigenvalues_S/total_var_S
prop_var_S
cum_var_S <- c()
for (i in seq_along(prop_var_S)){
  cum_var_S <- c(cum_var_S,sum(prop_var_S[c(1:i)]))
}

tableS <- round(cbind(eigenvalues_S,prop_var_S,cum_var_S),3)
colnames(tableS) <- c("Eigenvalue","Prop. of variance", "Cumul.prop.")
tableS
# This suggest keep 1 principle component, but for multivariate analysis, we keep two

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_S>mean(eigenvalues_S))
# this method also sugegs keep 1 principle component

# criteria 3, screeplot, look for a "bend" in the plot 

screeplot(prcomp(X) ,type="line")
plot(c(1:11), eigenvalues_S, type="b", xlab="Eigenvalue index", ylab="Variances", main="scree plot")
# Also suggest keep one, but for multivariate analysis, we keep two

# iii
# in this case, we only keep two principle component, and the eigenvectors are
eigenvectors_S[,1]
eigenvectors_S[,2]
# iv
# from the first eigenvector we see the 10th variable contributes the most to the principle component
# also the variance of the 10th variable is massive larger than the others
# from the second eigenvector we see variable 3, 6, 9, and 11 has significantly large value than the rest
# this corresponds to their variances e being larger than the the rest of the variables(other than 11)

# v
pc_S1 <- as.matrix(X)%*%eigenvectors_S[,1]
pc_S2 <- as.matrix(X)%*%eigenvectors_S[,2]
plot(pc_S1, pc_S2)
abline(v = 0, h = 0)
# the plots has some cluster new the origin, its doesn't fit ellipse shape well, not normal

# Analysis using covariance matrix R
spectral_decomp_R <- eigen(R,symmetric=T,only.values=F)
spectral_decomp_R
eigenvalues_R <- spectral_decomp_R$values
eigenvectors_R <- spectral_decomp_R$vectors
View(R)
round(eigenvalues_R, 5)
# i the first fractional contributions contributes to the majority of the variance 6/11
# but this time 2 and 1 also contributes a significant amount

# ii
# criteria 1, eigenvalues cumulative proportion table
prop_var_R <- eigenvalues_R/11
prop_var_R
cum_var_R <- c()
for (i in seq_along(prop_var_S)){
  cum_var_R <- c(cum_var_R,sum(prop_var_R[c(1:i)]))
}

tableR <- round(cbind(eigenvalues_R,prop_var_R,cum_var_R),3)
colnames(tableR) <- c("Eigenvalue","Prop. of variance", "Cumul.prop.")
tableR
# keeping three components gives us 84.3% of the variance

# criteria 2, compare each eigenvalue with the mean of eigenvalues
which(eigenvalues_R>mean(eigenvalues_R))
# this method also suggest us keep 3 components

# criteria 3, screeplot, look for a "bend" in the plot 
screeplot(prcomp(X) ,type="line")
plot(c(1:11), eigenvalues_R, type="b", xlab="Eigenvalue index", ylab="Variances", main="scree plot")
# the bends are not very significant in this one, the bend on the 3rd one is the most significant, suggest keeping 2

# overall, we will keep 3 principle, component

# iii
# in this case, we keep three principle components, and the eigenvectors are
eigenvectors_R[,1]
eigenvectors_R[,2]
eigenvectors_R[,3]


# iv
# in the 1st one, the contribution spread among all the variables almost evenly, showing linear dependence (time?)
# in the 2nd one, more contribution towards the latter half (location?)
# in the 3rd one, the 7th variable has the most distribution, in negative direction

# v
pc_R1 <- as.matrix(X)%*%eigenvectors_R[,1]
pc_R2 <- as.matrix(X)%*%eigenvectors_R[,2]
pc_R3 <- as.matrix(X)%*%eigenvectors_R[,3]
pc_R1; pc_R2; pc_R3
pcm_R <- cbind(pc_R1, pc_R2,pc_R3)
pcm_R
pairs(pcm_R,diag.panel = panel.hist)
# strong linear relationship between the 2nd and 3rd, not very useful
# 1st and 2nd or 3rd are not normal

# d
boxplot(X)
# R is the better choice here as there is scale issue where the range of variable is massively larger than the rest

# e
# Some assumption are made here, the the principal components suggest that some variables has more variance than others
# it's probably because of the location of the measurement?
# by looking at the PCA using R, principle component number one looks like it has something to do the time period of the temperature measurement
#