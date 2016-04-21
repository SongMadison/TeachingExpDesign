

# on page 380
dat1 <- data.frame(list(
    x1 = c(1,0,1,0,0,1,0),
    x2 = c(0,1,0,1,1,0,1),
    v = c(2,2.50,2.50,2.75,3.00,3.00,3.00),
    y = c(89, 97, 91, 98,100, 104, 97) 
    ))

lm1 <- lm(y ~ x1+x2+v -1, dat1)
summary(lm1)
summary(lm1)$coef
model.matrix(lm1)

lm2 <- lm(y~x1+x2+v,dat1,)
summary(lm2)
model.matrix(lm2)


# collinearity

# R tips:
# 1, offset, I
# These two functions are commonly used directly within a formula. Terms in a formula that should have coefficients fixed at 1 should be wrapped in offset. 
# Wrapping an expression (e.g. x1+x2) in I will make the expression be treated as a single variable in a formula, meaning it will get only a single coefficient estimate.
# offset(object)
# object – A variable in an equation.
# I(x)
# x – An object, often an expression of other objects.

# \log(\mu_i) = \log(N) + \beta_1 x_1 + \beta_2 x_2
# \log(\mu_i) = \log(N) + \beta (x_1 + x_2)
 Y  <- c(15,  7, 36,  4, 16, 12, 41, 15)
 N  <- c(4949, 3534, 12210, 344,  6178, 4883, 11256, 7125)
 x1 <- c(-0.1, 0, 0.2, 0,     1, 1.1, 1.1, 1)
 x2 <- c(2.2, 1.5, 4.5, 7.2,  4.5, 3.2, 9.1, 5.2)
 
glm(Y ~ offset(log(N)) + (x1 + x2), family=poisson)
# Coefficients:
#     (Intercept)           x1           x2  
# -6.172       -0.380        0.109  
# Degrees of Freedom: 7 Total (i.e. Null);  5 Residual
# Null Deviance:        10.56 
# Residual Deviance: 4.559 	AIC: 46.69 
 glm(Y ~ offset(log(N)) + I(x1+x2), family=poisson)
# Coefficients:
#     (Intercept)   I(x1 + x2)  
# -6.12652      0.04746  
# Degrees of Freedom: 7 Total (i.e. Null);  6 Residual
# Null Deviance:	    10.56 
# Residual Deviance: 8.001 	AIC: 48.13 