# Chapter 2: Probability and Distributions

# 2.1 Random sampling

sample(1:40,5)
sample(c("H","T"), 10, replace=T)
sample(c("succ", "fail"), 10, replace=T, prob=c(0.9, 0.1))

# 2.2 Probability calculations and combinatorics

1/prod(40:36)
prod(5:1)/prod(40:36)
1/choose(40,5)

# 2.5 The built-in distributions in R

x <- seq(-4,4,0.1)
plot(x,dnorm(x),type="l")
x <- 0:50
plot(x,dbinom(x,size=50,prob=.33),type="h")
1-pnorm(160,mean=132,sd=13)

pbinom(16,size=20,prob=.5)
1-pbinom(15,size=20,prob=.5)
1-pbinom(15,20,.5)+pbinom(4,20,.5)

xbar <- 83
sigma <- 12
n <- 5
sem <- sigma/sqrt(n)
sem
xbar + sem * qnorm(0.025)
xbar + sem * qnorm(0.975)

rnorm(10)
rnorm(10)
rnorm(10,mean=7,sd=5)
rbinom(10,size=20,prob=.5)
