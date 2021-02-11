data = read.table(
  file = 'C:/Users/John/Desktop/STAT 240/Data/pokemon_2019.csv',
  header = T,
  sep = ',',
  quote = '')
data
df = data.frame(matrix(c(a,b), nrow=4))
library(tidyverse)

#Problem 1
radius = c(1,3,5,7)
volume = 4/3*pi*radius^3
par(mfrow=c(2,2)) 
plot(radius,volume,main="Radius vs Volume for Sphere Line Plot",
     xlab="Radius",
     ylab="Volume",
     col = 'blue',
     lwd = 2,
     type = 'l',
     cex.main=0.8,
     ylim=c(0,1500),xlim=c(0,9))
plot(radius,volume,main="Radius vs Volume for Sphere Point Plot",
     xlab="Radius",
     ylab="Volume",
     col = 'red',
     lwd = 3,
     type = 'p',
     cex.main=0.8,
     ylim=c(0,1500),xlim=c(0,9))
plot(radius,volume,main="Radius vs Volume for Sphere No Plotting Plot ",
     xlab="Radius",
     ylab="Volume",
     col = 'green',
     lwd = 4,
     type = 'n',
     cex.main=0.8,
     ylim=c(0,1500),xlim=c(0,9))
plot(radius,volume,main="Radius vs Volume for Sphere Line and Point Plot",
     xlab="Radius",
     ylab="Volume",
     col = 'purple',
     lwd = 5,
     type = 'b',
     cex.main=0.8,
     ylim=c(0,1500),xlim=c(0,9))


?plot
#Problem 2
par(mfrow=c(1,1))
radius = seq(from = 1,to = 7,length = 10000)
volume = 4/3*pi*radius^3
plot(radius,volume,main="Volume vs Radius Line Plot",
     xlab="Radius",
     ylab="Volume",
     col = 'blue',
     lwd = 3,
     type = 'l',
     ylim=c(0,1500),xlim=c(-1,9))
library(tidyverse)
df = data.frame(radius,volume) 
ggplot(df, aes(x=radius,y=volume))+geom_line()+ggtitle("Volume vs Radius Line Plot")+
  theme(plot.title = element_text(size=15,hjust = 0.5))



#1.2 Adding more lines or points to a plot
plot(radius,volume,
     main="you should make a better title than this",
     xlab="this label needs to make sense",
     ylab="put something sensible here",type= 'l'
)
#Area
lines(1:10,pi*c(1:10)^2,col=2,lty = 2)
#Surface area
points(radius,4*pi*radius^2,col=3)
legend(x=3,y=1000,col=1:3,pch=c(NA,NA,1),lty = c(1,2,NA),
       c("Volume","Area","Surface Area"))
legend(x="topleft",col=1:3,pch=c(NA,NA,1),lty = c(1,2,NA),
       c("Volume","Area","Surface Area"))

#Problem 3
x = seq(from = -3, to = 3, length = 1000)
y1 = x^2
y2 = 2^x

plot(x,y1,main=expression("Graph for"~x^{2}~"and"~2^{x}),
     col=1,
     xlab="x-axis", 
     ylab="y-axis", 
     type= 'l',
     xlim=c(-3,3))
lines(x,y2,col= 2,lty = 2,lwd= 2)
legend(x=0,y=6,col=1:2,pch=c(NA,NA),lty = c(1,2), 
       c(expression(x^{2}),expression(2^{x})))

?legend

poke = read.csv(file = "pokemon_2019.csv",header=TRUE,sep = ",")
names(poke)
plot(poke[,"Type_1"], horiz=TRUE,las=2)
?plot
poke[,"HP"]>200
poke[poke[,"HP"]>200,"HP"]
which(poke[,"HP"]>200)
poke[poke[,"HP"]>200,]
plot(poke[,"Attack"] , poke[,"Defense"],
     ylab="Defense",xlab="Attack",
     col= poke[,"Generation"],pch= poke[,"Generation"],
     main="Make your own sensible title to go here")

#ggplot method for question 4b

po = read_csv(file = "pokemon_2019.csv")
po %>% 
  filter(Body_Style == 'head_arms' | Body_Style == 'serpentine_body') %>%
  ggplot(aes(x=Attack,y=Defense,color = Body_Style))+geom_point()

#Problem 4
#4a
poke[poke[,"Height_m"]>2 & poke[,"isLegendary"] == "True","Name"]
#4b
par(mfrow=c(1,1))
ha = poke[poke$Body_Style == 'head_arms',]
sb = poke[poke$Body_Style == 'serpentine_body',]
plot(ha$Attack,ha$Defense,main="Attack vs Defense",
     xlab="Attack",
     ylab="Defense",
     col = 2,
     type = 'p',
     ylim=c(10,210),xlim=c(10,160))
points(sb$Attack,sb$Defense,col=3)
legend(x="topleft",col=2:3,pch=c(1,1), c("Head Arms body type","Serpentine Body body type"))

pokenew = poke[1,]
pokenew$Name = "Charmandaver"
pokenew$Type_1 = "student"
pokemonextra = rbind(pokenew,poke)
plot(pokemonextra[,"Type_1"], las=2)

is.data.frame(poke)
is.data.frame(pokemonextra)
is.numeric(poke[,"Type_1"])
is.numeric(pokemonextra[,"Type_1"])
is.factor(poke[,"Type_1"])
is.factor(pokemonextra[,"Type_1"])
poke[1:5,"Type_1"]
pokemonextra[1:5,"Type_1"]
unique(poke[,"Type_1"])
pokemonextra[,"Type_1"] = factor(pokemonextra[,"Type_1"])
pokemonextra[1:5,"Type_1"]

as.numeric(poke[,"Type_1"])
factor(poke[,"Attack"])
levels(pokemonextra[,"Type_1"])
levels(poke[,"Type_1"])

A = function(B) { C = B + 7; return(C) }
A(22)
#Problem 5
#a + b
fib = function (x) {
  if (x == 0) {return (0)}
  else if (x == 1) {return (1)}
  else {return(fib(x-1)+fib(x-2)) }
}
sum = 0
for (i in 1:20 ) {
  sum = sum + fib(i)
}
sum
#c
# for log base 10 750489.79559545

tinytex::install_tinytex()






