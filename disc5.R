
rm(list = ls())
# Question 3
regimes  <- data.frame(list(
    Group  =  factor(rep(1:5, 3)) ,
    trt = rep(c("A","B","C"), each = 5),
    weightloss = c(15,24,31,37,33,10,15,28,36,37,8,17,34,34,39)
   ))
## graphical analysis:
library(ggplot2)
ggplot(data = regimes, aes(x= Group, y= weightloss, color = trt )) +
  geom_point()+
  geom_line()+
  ggtitle(label = "weightloss of different regimes")

# trt A seems have a good effect. all are similar though.
# some further test:

#consider group as block -- randomized complete block desgin
aov1 <- aov(weightloss ~ Group+ trt, data = regimes)
summary(aov1)




## if we know, reduced proportion
weights <- c(250,309,327,356,379)
regimes$weights <-rep(weights,3)
regimes$ratio <- regimes$weightloss/regimes$weights
aov2 <- aov(ratio ~ trt, data =regimes)
summary(aov2)

#conclusions : trt still not significant.

## if you want to calculate all kindso group means
library(dplyr)
group.mean <- regimes %>%
     dplyr::group_by(Group) %>%
     dplyr::summarize(mean(weightloss))
trt.mean <- regimes %>%
  dplyr::group_by(trt) %>%
  dplyr::summarize(mean(weightloss))

#graphial ANOVA
layout(matrix(1:3,3))
stripchart(group.mean$`mean(weightloss)`, ylim = c(0,4) ,col ="red",main="group mean")
stripchart(trt.mean$`mean(weightloss)`, ylim = c(0,4) ,method = 'jitter', col ="blue", main= "trt mean")
stripchart(aov1$residuals, main="residuals", method= "jitter")


library(ggplot2)
p1 <- ggplot(data.frame(x = group.mean$`mean(weightloss)`,y =rep(1,5)), aes(x,y)) + geom_point( )
p2 <- qplot(x,y, data= data.frame(x= trt.mean$`mean(weightloss)`,y = rep(2,3)), color = I("blue"))
p3 <- ggplot(data.frame(x= aov1$residuals, y = rep(1,15)), aes(x,y))+geom_point(color = "red")
library(gridExtra)
grid.arrange(p1,p2,p3)



### Question 4
process <- data.frame(list(
  runs = 1:32,
  block = factor(rep(1:8, each =4)),
  variants = unlist(strsplit("CBDABDACDABCADCBADBCDCABBDCACDAB", split="")),
  results = c(56,60,69,61,62,70,65,65,
             66,63,52,57,58,60,61,66,
             56,61,53,52,62,57,59,58,
             60,68,61,65,63,68,61,55)
  
))

aov2 <- aov(results ~ block + variants, data =process)
summary(aov2) 
#graphial ANOVA
layout(matrix(1:3,3))
stripchart(block.mean$`mean(results)` ,col ="red",main="group mean")
stripchart(variants.mean$`mean(results)`,method = 'jitter', col ="blue", main= "trt mean")
stripchart(aov2$residuals, main="residuals", method= "jitter")

## part C:
sigma.hat = sqrt ( sum(aov2$residuals^2)/21 )
t.crit <- qt(0.025, 21)
SE1 = t.crit * sigma.hat / sqrt(8) 


#confidence intervals for block
library(dplyr)
block.mean <- process %>%
  dplyr::group_by(block) %>%
  dplyr::summarize(mean(results))
variants.mean <- process %>%
  dplyr::group_by(variants) %>%
  dplyr::summarize(mean(results))
#variants (process):
data.frame(list('0.025' = variants.mean$`mean(results)`-SE1,
                '0.975' = variants.mean$`mean(results)`+SE1
))

#question d)
plot(aov2$residuals, main ="rediduals over time", ylab ="residuals")

#part (e)
plot(block.mean,ylab = "averages", xlab="time", main="averages over time")
lines(block.mean, col ="red")
## really big variation among those means of fun runs at different times


## Question 5
# don't think about the time/ blocking struture.
aov3 <- aov(results ~ variants, data = process)
summary(aov3)

sigma.hat2 = sqrt ( sum(aov3$residuals^2)/28 )
t.crit2 <- qt(0.025, 28)
SE2 = t.crit2 * sigma.hat2 / sqrt(8) 


#variants (process):
data.frame(list('0.025' = variants.mean$`mean(results)`-SE2,
                '0.975' = variants.mean$`mean(results)`+SE2
))

SE1/SE2
