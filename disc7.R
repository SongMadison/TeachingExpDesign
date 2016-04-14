
rm(list= ls())
dat1 <- data.frame(list(
  temp = rep(c(-1,1),4),
  concen = rep(c(-1,-1,1,1),2),
  stir = rep(c(-1,1),each =4),
  yield = c(54,57,69,70,55,54,80,81)
))

lm1 <- lm(yield~.,data= dat1)
summary(lm1)

lm2 <- lm(yield~temp+ concen+ stir + temp:concen+ temp:stir + concen:stir,data= dat1)
summary(lm2)

lm3 <- lm(yield ~ temp *concen * stir, data =dat1)
summary(lm3)
#using the knowledge that eror is ==1, sd =(1/8+1/8)^(1/2) = 1/2

# a) conlusions 
# main effects: concentrain effect are significant

# the population std are given, =1
# based on saturated model, using sd for each effect  = 1 *0.5, effects >1 should be significant
# so concen, stir, concen:stir are significant.
# they all have positive effects

# b) perform experiments with comnibations of high concentraion, high stirring rate, 
# and possibly temperature.



## question 15 from textbook.
rm(list= ls())
dat1 <- data.frame(list(
  temp = rep(c(-1,1),4),
  concen = rep(c(-1,-1,1,1),2),
  time = rep(c(-1,1),each =4),
  yield = c(80,65,69,74,81,84,91,93)
))
dat2 <- data.frame(list(
  temp = rep(c(-1,1),4),
  concen = rep(c(-1,-1,1,1),2),
  time = rep(c(-1,1),each =4),
  yield = c(62,63,73,80,79,86,93,93)
))
data <- rbind(dat1,dat2)

model1 <- lm (yield~temp*concen*time, data)
summary(model1)


model2 <- lm (yield~temp*concen*time, data[-1,])
summary(model2)
X2 <- model.matrix(model2)
t(X2)%*%X2



attach(data)
interaction.plot(temp,concen,yield,type="b",pch =1:2, lty= 1:2,
                 lwd =2, cex =2, cex.lab= 1.3)
title(main="plot of cell means")
detach(data)