setwd("C:/Users/John/Desktop/STAT 445/Data")
library(readxl)
library(psych)
library(forecast)
library(MESS)
protein <- read_excel("europe_protein_data.xls")
protein <- protein[,-1]
protein
R <- cor(protein)


m1 <- fa(R,nfactors=4, n.obs=25, rotate="none", fm="ml")
estimate1 <-m1$loadings%*%t(m1$loadings)+diag(m1$uniquenesses)
error1 <- R-estimate1
sqrt(sum(error1^2))
round(error1,4)
round(R,4)

round(error1/R,4)



n1 <- factanal(protein, factors = 4, rotation = "none")
es1 <- n1$loadings%*%t(n1$loadings)+diag(n1$uniquenesses)
er1 <- R-es1
sqrt(sum(er1^2))



m2 <- fa(R,nfactors=4, n.obs=25, rotate="varimax", fm="ml")
estimate2 <-m2$loadings%*%t(m2$loadings)+diag(m2$uniquenesses)
error2 <- R-estimate2
sqrt(sum(error2^2))


n2 <- factanal(protein, factors = 4, rotation = "varimax")
es2 <- n2$loadings%*%t(n2$loadings)+diag(n2$uniquenesses)
er2 <- R-es2
sqrt(sum(er2^2))





a <- principal(R,nfactors=4, n.obs=25, rotate="none")
esa <-a$loadings%*%t(a$loadings)+diag(a$uniquenesses)
era <- R-esa
era
mean(abs(era/R))

mean(abs(error1/R))

sqrt(sum(era^2))
sqrt(sum(error1^2))

sqrt(sum(R^2))

