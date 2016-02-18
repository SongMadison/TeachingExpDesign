rm(list=ls())  # clean up work space
setwd("/Users/SongWang/Stat/Teaching/stat424/code/project1")

Plastic <- read.csv("Project 1 data.csv")

# question 1: delete one row: last number in my ID is 9
Plastic <- Plastic[-9,]

# question 2:
#part a)
Plastic_ReactorA <- Plastic[which(Plastic$Reactor=='A'),]
Plastic_ReactorB <- Plastic[which(Plastic$Reactor=='B'),]

mean(Plastic_ReactorA$Temp)
mean(Plastic_ReactorB$Temp)
mean(Plastic$Temp)

mean(Plastic_ReactorA$Conv)
mean(Plastic_ReactorB$Conv)
mean(Plastic$Conv)

#part b): correlation coefficient between temperature and conversion, overall and by reactor
cor(Plastic$Temp,Plastic$Conv)
cor(Plastic_ReactorA$Temp, Plastic_ReactorA$Conv)
cor(Plastic_ReactorB$Temp, Plastic_ReactorB$Conv)

# part c): Comparative histograms of temperature and conversion, by reactor
layout(matrix(c(1,2)))
hist(Plastic_ReactorA$Temp)
hist(Plastic_ReactorB$Temp)
hist(Plastic_ReactorA$Conv)
hist(Plastic_ReactorB$Conv)


#part d):  A scatter plot of y = conversion vs. x = temperature, 
# using all the data and symbols denoting reactor

layout(1) # change back to one plot a page
plot(Plastic$Temp, Plastic$Conv, col = Plastic$Reactor, pch = as.numeric(Plastic$Reactor))
title("scatter plot temp vs. conv")
legend('topleft', legend = c("reactor A", "reactor B"), col = c(1,2), pch = c(1,2))


#part e): A normal probability plot of the deviations about the individual reactor means
mean.vec <- (Plastic$Reactor =='A') * mean(Plastic_ReactorA$Conv)  +  
           (Plastic$Reactor =='B') * mean(Plastic_ReactorB$Conv)
deviations <- Plastic$Conv - mean.vec
qqnorm(deviations)
qqline(deviations)

qqnorm(devA <- Plastic_ReactorA$Conv-mean(Plastic_ReactorA$Conv) );qqline(devA)
qqnorm(devB <- Plastic_ReactorB$Conv-mean(Plastic_ReactorB$Conv) );qqline(devB)
# Question 3: Compare the outputs and report any interesting findings. 
# This report should be very brief, just a few sentences.

#1, From the histogram, compare with reactor A, the conversion rate of reactor B is more spread; 
   # and they tend to take big or small values
#2, There is a strong linear relationship between conversion rate and temperature for reactor A
    # this is not the case for reactor B, for the same temperature,
     #there are types of different resutls.
#3, when put the deviations from reactor A and reactor B together, some of the points are clearly
   # deviate from the line and so the normality is not satisfied in this case.

