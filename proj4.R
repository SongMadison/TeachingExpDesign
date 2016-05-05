
rm(list=ls())
#create table from scratch
dat1 <- data.frame(list(
    I = rep(1,8),
    A = rep(c(-1,1),4),
    B= rep(c(-1,-1,1,1), 2),
    C= rep(c(-1,1), each =4)
))
dat1$AB <- dat1$A*dat1$B
dat1$AC <- dat1$A*dat1$C
dat1$BC <- dat1$B*dat1$C
dat1$D <- dat1$A*dat1$B*dat1$C

dat1 <- rbind(dat1,-dat1)

dat1 <- rbind(dat1,c(1,0,0,0,0,0,0,1) )
dat1 <- rbind(dat1,c(1,0,0,0,0,0,0,-1) )
dat1 <- rbind(dat1,c(1,0,0,0,0,0,0,-1) )
dat1 <- rbind(dat1,c(1,0,0,0,0,0,0,1) )

dat1 <- rbind(dat1,c(1,2,0,0,0,0,0,1))
dat1 <- rbind(dat1,c(1,-2,0,0,0,0,0,1))
dat1 <- rbind(dat1,c(1,-2,0,0,0,0,0,-1))
dat1 <- rbind(dat1,c(1,2,0,0,0,0,0,-1))

dat1 <- rbind(dat1,c(1,0,2,0,0,0,0,1))
dat1 <- rbind(dat1,c(1,0,-2,0,0,0,0,1))
dat1 <- rbind(dat1,c(1,0,2,0,0,0,0,-1))
dat1 <- rbind(dat1,c(1,0,-2,0,0,0,0,-1))

dat1 <- rbind(dat1,c(1,0,0,2,0,0,0,1))
dat1 <- rbind(dat1,c(1,0,0,-2,0,0,0,1))
dat1 <- rbind(dat1,c(1,0,0,2,0,0,0,-1))
dat1 <- rbind(dat1,c(1,0,0,-2,0,0,0,-1))

## interaction terms and quadratic terms
dat2 <- dat1
dat2$BD <- dat2$B * dat2$D
dat2$A2 <- dat2$A^2
dat2$B2 <- dat2$B^2
dat2$C2 <- dat2$C^2

## beta 
beta <- matrix(c(20, 8,8,8,0,0,0,8,10,5,5,5))
y <- as.matrix(dat2)%*% beta + rnorm(nrow(dat2))

dat2$y <- y


## check X^TX
X <- as.matrix ( dat2[,1:12])
solve(t(X)%*%X)


## substitube with real values in design
#A  50, 90 B, 10, 20, C 0.3, 0.7
dat1.real <- dat1
dat1.real$A[which(dat1.real$A==-1)] <- 50; dat1.real$A[which(dat1.real$A==1)] <- 90
dat1.real$A[which(dat1.real$A==-2)] <- 30; dat1.real$A[which(dat1.real$A==2)] <- 110
dat1.real$A[which(dat1.real$A==0)] <- 70;
dat1.real$B[which(dat1.real$B==-1)] <- 10; dat1.real$B[which(dat1.real$B==1)] <- 20
dat1.real$B[which(dat1.real$B==-2)] <- 5; dat1.real$B[which(dat1.real$B==2)] <- 25
dat1.real$B[which(dat1.real$B==0)] <- 15;
dat1.real$C[which(dat1.real$C==-1)] <- 0.3; dat1.real$C[which(dat1.real$C==1)] <- 0.7
dat1.real$C[which(dat1.real$C==-2)] <- 0.1; dat1.real$C[which(dat1.real$C==2)] <- 0.9
dat1.real$C[which(dat1.real$C==0)] <- 0.5;
### used the dat in excel
data <- read.csv("proj4.csv")
data$D[which(data$D==-1)]=0
data <- data[,c(2:4,8,12)]
data.c <- data
data.c$A <- data.c$A- mean(data.c$A)
data.c$B <- data.c$B- mean(data.c$B)
data.c$C <- data.c$C- mean(data.c$C)
# Q3:
model1 <- lm(y ~  A+B+C+D + B:D, data)
summary(model1)
anova(model1)

model1.c <- lm(y ~ A+B+C+D+ B:D, data.c)
summary(model1.c)

# Q4:
model1 <- lm(y ~  A+B+C+D + B:D, data)
summary(model1)
library(car)
vif(model1)  ## check multicollinearity

vif(model1.c)

X <- model.matrix(model1)
t(X)%*%X
X <- model.matrix(model1.c)
t(X)%*%X


# Q6, 

