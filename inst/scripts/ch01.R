# Chapter 1: Basics 

# 1.1 First steps

plot(rnorm(500))

2 + 2
exp(-2)
rnorm(15)
x <- 2
x
x + x

weight <- c(60, 72, 57, 90, 95, 72)
weight
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2
bmi

sum(weight)
sum(weight)/length(weight)
xbar <- sum(weight)/length(weight)

weight - xbar
(weight - xbar)^2
sum((weight - xbar)^2)
sqrt(sum((weight - xbar)^2)/(length(weight) - 1))

mean(weight)
sd(weight)

t.test(bmi, mu=22.5)

plot(height,weight)
plot(height, weight, pch=2)
hh <- c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2)

# 1.2 R language essentials

args(plot.default)

c("Huey","Dewey","Louie")
c('Huey','Dewey','Louie')
c(T,T,F,T)
bmi > 25
c(42,57,12,39,1,3,4)

seq(4,9)
seq(4,10,2)
4:9
oops <- c(7,9,13)
rep(oops,3)
rep(oops,1:3)
rep(1:2,c(10,15))

x <- 1:12
dim(x) <- c(3,4)
x
matrix(1:12,nrow=3,byrow=T)
x <- matrix(1:12,nrow=3,byrow=T)
rownames(x) <- LETTERS[1:3]
x
t(x)
cbind(A=1:4,B=5:8,C=9:12)
rbind(A=1:4,B=5:8,C=9:12)

pain <- c(0,3,2,2,1)
fpain <- factor(pain,levels=0:3)
levels(fpain) <- c("none","mild","medium","severe")
fpain
as.numeric(fpain)
levels(fpain)
text.pain <-  c("none","severe", "medium", "medium", "mild") 
factor(text.pain)

intake.pre <- c(5260,5470,5640,6180,6390,
6515,6805,7515,7515,8230,8770)
intake.post <- c(3910,4220,3885,5160,5645,
4680,5265,5975,6790,6900,7335)
mylist <- list(before=intake.pre,after=intake.post)
mylist
mylist$before

d <- data.frame(intake.pre,intake.post)
d
d$intake.pre

intake.pre[5]
intake.pre[c(3,5,7)]
v <- c(3,5,7)
intake.pre[v]
intake.pre[1:5]
intake.pre[-c(3,5,7)]
intake.post[intake.pre > 7000]
intake.post[intake.pre > 7000 & intake.pre <= 8000]
intake.pre > 7000 & intake.pre <= 8000
d <- data.frame(intake.pre,intake.post)
d[5,1]
d[5,]

d[d$intake.pre>7000,]
sel <- d$intake.pre>7000
sel
d[sel,]

data(thuesen)
thue2 <- subset(thuesen,blood.glucose<7)
thue2
thue3 <- transform(thuesen,log.gluc=log(blood.glucose))
thue3

data(energy)
energy
exp.lean <- energy$expend[energy$stature=="lean"]
exp.obese <- energy$expend[energy$stature=="obese"]
l <- split(energy$expend, energy$stature)
l

intake.post
sort(intake.post)
order(intake.post)
o <- order(intake.post)
intake.post[o]
intake.pre[o]

lapply(thuesen, mean, na.rm=T)
sapply(thuesen, mean, na.rm=T)
m <- matrix(rnorm(12),4)
m
apply(m, 2, min)
tapply(energy$expend, energy$stature, median)

# 1.3 The graphics subsystem

x <- runif(50,0,2)
y <- runif(50,0,2)
plot(x, y, main="Main title", sub="subtitle",
     xlab="x-label", ylab="y-label")
text(0.6,0.6,"text at (0.6,0.6)")
abline(h=.6,v=.6)
for (side in 1:4) mtext(-1:4,side=side,at=.7,line=-1:5)
mtext(paste("side",1:4), side=1:4, line=-1,font=2)

plot(x, y, type="n", xlab="", ylab="", axes=F)

points(x,y)
axis(1)
axis(2,at=seq(0.2,1.8,0.2))
box()
title(main="Main title", sub="subtitle",
    xlab="x-label", ylab="y-label")

x <- rnorm(100)
hist(x,freq=F)
curve(dnorm(x),add=T)  

h <- hist(x, plot=F)
ylim <- range(0, h$density, dnorm(0))
hist(x, freq=F, ylim=ylim)
curve(dnorm(x), add=T)  

# 1.4 R programming

hist.with.normal <- function(x, xlab=deparse(substitute(x)),...)
{
    h <- hist(x, plot=F, ...)
    s <- sd(x)
    m <- mean(x)
    ylim <- range(0,h$density,dnorm(0,sd=s))
    hist(x, freq=F, ylim=ylim, xlab=xlab, ...)
    curve(dnorm(x,m,s), add=T)
}
hist.with.normal(rnorm(200))

y <- 12345

x <- y/2 
while (abs(x*x-y) > 1e-10) x <- (x + y/x)/2
x
x^2  

x <- y/2 
repeat{ 
    x <- (x + y/x)/2
    if (abs(x*x-y) < 1e-10) break
}
x

x <- seq(0, 1,.05)
plot(x, x, ylab="y", type="l")
for ( j in 2:8 ) lines(x, x^j)

t.test(bmi, mu=22.5)$p.value

print
length(methods("print")) # quoted in text

# 1.5 Session management

ls()
rm(height, weight, bmi)
attach(thuesen)
blood.glucose
search()
detach()
search()

# 1.6 Data entry

# Note: You should ensure that the input file is at the specified 
#       location or adjust the path. The file is contained in the
#       data directory of the ISwR package

thuesen <- read.table("N:/ISwR/thuesen.txt",header=T)
thuesen

data(airquality)
aq <- edit(airquality)

fix(airquality)
levels(airquality$Month) <- c("May", "June", "July",
                            "August", "September")

dd <- data.frame()
fix(dd)
