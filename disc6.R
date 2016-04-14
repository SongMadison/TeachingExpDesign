```{r}
rm(list =ls())

#Part 1,  verify the result in textbook page 210.
dat1 <- data.frame(list(
  O = c(-1,1,-1,1,-1,1,-1,1),
  H = c(-1,-1,1,1,-1,-1,1,1),
  C = c(-1,-1,-1,-1,1,1,1,1),
  y = c(5.9,4.0,3.9,1.2,5.3,4.8,6.3,0.8)
))

lm1 <- lm(y~O*H*C,data = dat1)
summary(lm1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    4.025         NA      NA       NA
# O             -1.325         NA      NA       NA
# H             -0.975         NA      NA       NA
# C              0.275         NA      NA       NA
# O:H           -0.725         NA      NA       NA
# O:C           -0.175         NA      NA       NA
# H:C            0.225         NA      NA       NA
# O:H:C         -0.525         NA      NA       NA
X <- model.matrix(lm1)
solve(t(X)%*%X)%*%t(X)%*%dat1$y

###################################################
#relationship with respect to effects in textbook
####################################################
#1 ,  main effect, interaction effects estimates in textbook are twice the 
# number returned by the fitting
# mean = 4.025 , O = 2.65, H = -1.95 ...

#2, standard deviation from effects in text book doubled, too.



## Comments : better run through hand calculation




lm2 <- lm(y~O+H+C,data = dat1)
summary(lm2)
## intesting observation, all std are the same.
## because  t(X)%*%X = 8 * identity


#Part 2, 
library (BHH2) ## the library with our text book.
library(xtable)  ## can help create table from R for latex to use.
xtable(dat1)

```