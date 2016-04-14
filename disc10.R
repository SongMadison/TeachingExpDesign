rm(list=ls())
library(car) #boxCox(lmObj)
library(BHH2) # graphical anova

dat1 <- data.frame(list(
  Machines =rep(c("A","B","C"),5),
  Materials = as.factor(rep(1:5, each=3)),
  values =c( 382,427,399,151,167,176,182,209,320,85,123,127,40,52,75)
))

aov11 <- aov(values~ Machines + Materials, dat1)
summary(aov11)
anovaPlot(aov11, main = "Anova plot untransformed",
          labels = TRUE, cex.lab = 0.6)
par(mfrow=c(2,2))
plot(aov11,which = 1)
plot(aov11,which = 2)
plot(aov11,which = 3)
plot(aov11,which = 4)
par(mfrow=c(1,1))

library(car)
lambda <- boxCox(aov11)
lambda_max <- lambda$x[which(lambda$y==max(lambda$y))] 
#close to 0, use logrithm transformation

y_lambda <- log(values)
aov.ex2 <- aov(log(values)~., dat1)
plot(aov.ex2,which=1)
plot(aov.ex2,which=2)
summary(aov.ex2)

anovaPlot(aov.ex2, main = "Anova plot",
          labels = TRUE, cex.lab = 0.6)




#problem4 
dat2 <- data.frame(list(
  Machines =rep(c("A","b","C","D"),4),
  Plastic = factor( rep(1:4, each =4 )),
  values =c(1271,4003,1022,2643,1440,1651,372,5108,
            612,1664,829,1944,605,2001,258,1607)
))
lambda <- seq(-2,2,by = 0.01)
Fvalues <- matrix(0,nrow=length(lambda),ncol=2)

for( i in 1:length(lambda) ){
  lab <- lambda[i]
  y <- dat2$values
  if (lab >0 | lab<0 ) {
    y.lab <- (y^lab-1)/lab
  }else {
    y.lab <- log(y)
  }
  
  aov.lab <- aov(y.lab ~ Machines+ Plastic, data = dat2)
  Fvalues [i,]<- summary(aov.lab)[[1]]$'F value'[1:2]
}

plot(lambda,Fvalues[,1], pch = "+", type ="b")
plot(lambda,Fvalues[,2],, pch = "+", type ="b")
(lab.max <- lambda[ which(Fvalues[,1]==max(Fvalues[,1]))])  
(lab.max2 <- lambda[which(Fvalues[,2]==max(Fvalues[,2]))])

## logrithm on the response
aov.ex2 <- aov(log(values)~.,data=dat2)
summary(aov.ex4)
anovaPlot(aov.ex4)
par(mfcol=(c(1,1)))
plot(aov.ex4, which=1)
plot(aov.ex4, which=2)

## C2,D2 can be bad points or ouliers. or there are interactions




###problem 6,
dat3 <- data.frame(list(
  values = c(1020,155,1550,75,1430,550,340, 25,
             1390,410,570,5,2950,160,730,20),
  A = rep(c(-1,1),8),
  B = rep(c(-1,-1,1,1),4),
  C = rep(c(-1,-1,-1,-1,1,1,1,1),2),
  D = c(rep(-1,8),rep(1,8))
))
# with(dat3, (dat3$E =A*B*C*D))
dat3$E <- with(dat3, (dat3$E =A*B*C*D))
aov31 <- aov(values~.,dat3)
plot(aov31) ## variance is increasing
boxCox(aov31)
## colse to zero, logrithm 
aov32 <- aov(log(values)~., dat3)
plot(aov32)

formula <- log(values) ~ A+B+C+D+E+A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+C:E+D:E
aov33 <- aov(formula, dat3)
summary(aov33)


#there is still some patterns in the residual plot, explore some possible interaction
with(dat3,interaction.plot(
  A,B,log(values), fixed = TRUE, col = 2:3, type = "l", leg.bty = "o"
))
with(dat3,interaction.plot(
  A,E,log(values), fixed = TRUE, col = 2:3, type = "l", leg.bty = "o"
))
?interaction.plot(x.factor, y.factor, response)

aov.ex3 <- aov(log(values)~A+B+C+D+E+A:B+A:D+A:E,dat3)
par(mfrow=c(2,2))
plot(aov.ex3,which=c(1:4))

## still not very good. equal variance, not normal