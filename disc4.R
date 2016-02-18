wear <- c(69,83,74,61,78,69,59,65,64,52,71,64,55,65,59,59,67,58,70,75,74,62,74,74)
paint=list()
paint$supplier <- c(rep("GS",6),rep("FD",6),rep("L",6),rep("zK",6))
paint$site <- rep(1:6,4)
paint$wear <- wear
paint <- data.frame(paint) 



#graphical analysis
plot(paint$wear[1:6],rep(1,6), col="red",ylim=c(0.5,4.5), xlim= range(paint$wear))
points(paint$wear[7:12], rep(2,6),  col="blue")
points(paint$wear[13:18], rep(3,6), col="green")
points(paint$wear[19:24], rep(4,6))

layout(1)
boxplot(wear~supplier,data =paint)
# pontentially ,there is one outliter
boxplot(wear~site, data = paint)
plot(wear ~ site, data = paint)


paint$site <- as.factor(paint$site) # change site from int type to factor type
aov1 <- aov(wear~supplier+site, data = paint)
summary(aov1)
aov1$effects
aov1$coefficients

plot(aov1 ,which = 1)  # residuels vs fitted
# we can that there is no trend of residuals, which all flutate around 0.
# this can support the indepence assumption and the common variance assumpiton
plot(aov1, which =2)   # norm Q-Q
# no dlear outlier, normal assumption approx. satisfied.
plot(aov1, which =3)  # use to see if there are outliers or not

layout(matrix(1:4,2, byrow = T))
plot(aov1)
##draw by yourself 
qqnorm(aov1$resid,col = paint$supplier)
qqline(aov1$residuals)


### extra questions:
## use dplyr to calculate different group means
require(dplyr)
mean.wear.by.supplier = paint %>% 
  dplyr::group_by(supplier) %>%
  dplyr::summarise(mean(wear))
mean ( paint[which(paint$supplier=="GS"),]$wear)
mean.wear.by.supplier 
mean.wear.by.site = paint %>% 
  dplyr::group_by(site) %>%
  dplyr::summarise(mean(wear))

paint %>% 
  dplyr:: group_by(site) %>% 
  dplyr::summarize(mean(wear))
