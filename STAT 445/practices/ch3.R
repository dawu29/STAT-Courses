library(tidyverse)
library(readxl)
library(psych)
library(matrixStats)
library(matlib)

# 1 

X <- matrix(c(9,5,1,1,3,2), nrow=3, ncol=2, byrow = F)
X

x_bar <- colMeans(X)
x_bar

one <- c(1,1,1)
x_bar[1]
#y1-x_bar1 * vec 1
#y1 is the first column matrix X
z1 <- X[,1] - x_bar[1]*one
z1
#y2-x_bar1 * vec 1
z2 <- X[,2] - x_bar[2]*one
z2

#norm of those two
sqrt(sum(z1*z1))
sqrt(sum(z2*z2))

#angle

costheta <- sum(z1*z2)/ (sqrt(sum(z1*z1)) * sqrt(sum(z2*z2)))
acos(costheta)


# 2

park <- read_excel("C:/Users/John/Desktop/STAT 445/Data/NationalParks.xlsx")
park <- as.matrix(park[,2:3])
park

park_bar <- colMeans(park)
park_bar
park_one <- rep(1, 15)

#x_bar2 * vec 1
park_bar[2]*park_one

#y2 - x_bar2 * vec 1
park[,2] - park_bar[2]*park_one


# 3
X <- matrix(c(1,2,4,8,16,-0.5,-1,-2,-4,-8,2,4,8,16,32), nrow = 5, ncol = 3, byrow = F)
X
# a)
cor(X)
# the columns of X are linearly dependent, eg, col3 = col1 -2*col2
# we can also see this in the correlation matrix
# b) We might consider removing the cumulative winnings column(total winnings)


# 4
# a
X <- matrix(c(-1,2,5,3,4,2,-2,2,3), nrow = 3, ncol = 3, byrow = F)
X

x_bar <- colMeans(X)
S <- cov(X) # unbiased
S
Sn <- (2/3) * S # biased
Sn
det(S) # ageneral sample variance is about zero

# b
# in S, col3 is col1 + col2, linearly dependent - > det(S) = 0 -> the volume of parallelepipe = 0



# 5 
X <- matrix(c(5,3,-1,2,4,2,2,1,3,6,-1,1,3,4,8), nrow = 5, ncol = 3, byrow = F)
X

zero <- rep(0, 5)
zero
# a
solve(X, zero)

#b
S <- cov(X)
det(S)

#c


# 6
ntr <- read_excel("C:/Users/John/Desktop/STAT 445/Data/w-nat-track-rec.xlsx", col_names = F)
ntr

# a
ntr_bar <- colMeans(ntr)
ntr_bar
Sx <- cov(ntr)
Sx


