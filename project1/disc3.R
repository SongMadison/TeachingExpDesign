rm(list=ls())
#1 t- test
A <- c(67,80,106,83,89)
B <- c(45,71,87,53)

#calculated by hand
mean.A <- mean(A)
sd.A <- sd(A)
n.A = length(A)

mean.B <- mean(B)
sd.B <- sd(B)
n.B = length(B)

sd.pooled2 <- (sd.A^2*(n.A-1)+sd.B^2*(n.B-1))/(n.A+n.B-2) 
sd.pooled = sqrt(sd.pooled2)
t.value = (mean.A-mean.B)/(sd.pooled*(1/n.A+1/n.B)^(1/2))
t.value

df = n.A+n.B -2


pvalue = 2*(1- pt(t.value,df = df))
pvalue

t.test(A,B,var.equal = T)

## weltch's t test, for unequal variance
t2 = (mean.A -mean.B) / sqrt(sd.A^2/n.A+sd.B^2/n.B)
t2
df2 =  (sd.A^2/n.A+sd.B^2/n.B)^2 / (sd.A^4/n.A^2/(n.A-1)+sd.B^4/n.B^2/(n.B-1))
df2
2*(1 - pt(t2, df2))
t.test(A,B)  

## from wikipedia : subtle difference:
# In statistics, Welch's t-test (or unequal variances t-test) is a two-sample location test,
# and is used to test the hypothesis 
# that two populations have equal means. Welch's t-test is an adaptation of Student's t-test,
# and is more reliable when the two samples have unequal variances and unequal sample sizes.
# These tests are often referred to as "unpaired" or "independent samples" t-tests, 
# as they are typically applied when the statistical units underlying the two samples being 
# compared are non-overlapping.


## problem 2 
x <- c(1.4, 1.2, 1.2, 1.3, 1.5, 1.0, 2.1,1.4,1.1)
qqnorm(x)
qqline(x)
s2 <- var(x)
n.x = length(x)
sigma =0.4
chi.value <- s2/sigma^2
chi.value
2*(pchisq(chi.value,n.x-1))


## in-buit test
chisq.test(x)
# data:  x
# X-squared = 0.60656, df = 8, p-value = 0.9997
# 
# Warning message:
#   In chisq.test(x) : Chi-squared approximation may be incorrect

#delete potential outlier 2.1
x.new <- x[-which(x==2.1)]
qqnorm(x.new)
qqline(x.new)
chisq.test(x.new)
