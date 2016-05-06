

# design factors A B C D    2^(4-1)
# enviroment factors , 2^(3-1)
rm(list=ls())
dat1 <- data.frame(list(
    A = rep(c(-1,1),4),
    B= rep(c(-1,-1,1,1), 2),
    C= rep(c(-1,1), each =4)
))
dat1$D <- dat1$A*dat1$B*dat1$C
dat2 <- data.frame(list(
    S = c(-1,1,-1,1),
    H = c(-1,-1,1,1)
))
dat2$R <- dat2$S*dat2$H


data <- rbind(dat1,dat1,dat1,dat1)
dat2.vec<- as.vector(t(dat2))
dat2.vec <- c( rep(dat2.vec[1:3],8),rep(dat2.vec[4:6],8), 
               rep(dat2.vec[7:9],8),rep(dat2.vec[10:12],8) )
dat2.extend <- matrix(dat2.vec,ncol =3, byrow = T)

data <- cbind(data, dat2.extend)
names(data)[5:7] <- c("S","H","R")
data$y <- c(88,80,90,95,84,85,91,89,
            85,77,84,87,82,84,93,88,
            88,80,91,93,83,82,92,89,
            85,76,86,88,84,82,92,87)
write.csv(data, file="envr_robust.csv", row.names = F)
model1 <- lm(y ~ A+B+C+D+S+H+R, data )
anova(model1)


model2 <- lm(y ~ A+B+C+D+S+H+R + A:B+A:C+B:C + A:S+A:H+A:R+ 
                 B:S+B:H+B:R + C:S+C:H+C:R + D:S+D:H+D:R, data )
X2<- model.matrix(model2)
summary(model2)
anova(model2)

model3 <- lm(y ~ A+B+C+D+S+H+R + A:B+A:C+B:C +
                 A:S+A:H+A:R+  B:S+B:H+B:R + C:S+C:H+C:R + D:S+D:H+D:R+
                 A:B:S+A:B:H+A:B:R+  B:C:S+B:C:H+B:C:R + A:C:S+A:C:H+A:C:R, data )
X <- model.matrix(model3)
t(X)%*%X
summary(model3)
anova(model3)

data3 <- list()
data3$design <- as.factor(rep(1:8,4))
data3$environment <- as.factor(rep(1:4, each =8))
data3$y <- data$y

model4 <- lm(y~design+environment, data3)
anova(model4)

model5 <- lm(y~design*environment, data3)
anova(model5)
summary(model5)

##  data  on page 540 of textbook
dat <- data.frame(list(
    A = c(1,1,-1,-1,1,-1,-1,1),
    B = c(-1,-1,-1,-1,1,1,1,1),
    C = c(-1,-1,1,1,1,1,-1,-1),
    D = c(1,1,-1,-1,-1,1,1,-1),
    E = c(1,-1,1,-1,1,1,-1,-1)
))
data4 <- rbind(dat,dat,dat,dat,dat,dat)
data4 <- cbind(data4, matrix(c(rep(c(-1,-1),8),rep(c(0,-1),8), rep(c(1,-1),8),rep(c(-1,1),8),
                               rep(c(0,1),8),rep(c(1,1),8)),ncol=2, byrow = T))
names(data4)[6:7]<- c("R","S")
data4$y <- c(8,7,3,7,0,4,9,2,  4,2,2,1,0,0,1,1,    3,1,1,1,0,0,0,3,  0,4,1,2,0,1,4,1,
             4,5,0,0,0,1,5,0,  7,9,0,4,1,1,1,0)

model.4 <- lm(y ~A+B+C+D+E+R+S, data4)
X4 <- model.matrix(model.4)
t(X4)%*%X4

formula = "y ~ A+B+C+D+E+R+S + A:B+ A:C+ A:D+A:E + B:C+ B:D + B:E+ C:D+C:E+ D:E + R:S +
             A:R+ B:R+ C:R+D:R+E:R+ A:S+B:S+C:S+D:S+E:S"
model.42 <- lm(formula, data4)
summary(model.42)
