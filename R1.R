x <- 0:6
freq <- c(0,4,19,15,17,7,2)
x.mean <- sum(x*freq)/sum(freq)

x.var <- sum((x-x.mean)^2*freq)/sum(freq-1)


z.test <- (x.mean-2)/sqrt(x.var)

p.value <- 1-pnorm(0.94)


varSimu<-{}
N=1000
for (i in 1:N){
  sample = rbinom(64,6,1/3)
  #rbinom(n, size, prob)
  varSimu <- c(varSimu, var(sample))
}

p.value.Simu <- sum(varSimu>x.var)/N

hist(varSimu, main= paste("Based on resampling, pvalue =",round( p.value.Simu,3)))
abline( v=x.var, col ="red")

probs <- c( dbinom(0,6,1/3),
dbinom(1,6,1/3),
dbinom(2,6,1/3),
dbinom(3,6,1/3),
dbinom(4,6,1/3),
dbinom(5,6,1/3),
dbinom(6,6,1/3))
64*probs


