# Chapter 3: Descriptive statistics and graphics

# 3.1 Summary statistics for a single group

x <- rnorm(50)
mean(x)
sd(x)
var(x)
median(x)

quantile(x)
pvec <- seq(0,1,0.1)
pvec
quantile(x,pvec)

data(juul)
attach(juul)
 mean(igf1)
mean(igf1,na.rm=T)
sum(!is.na(igf1))
summary(igf1)
summary(juul)

detach(juul)
juul$sex <- factor(juul$sex,labels=c("M","F"))            
juul$menarche <- factor(juul$menarche,labels=c("No","Yes"))
juul$tanner <- factor(juul$tanner,
                      labels=c("I","II","III","IV","V"))
attach(juul)
summary(juul)

# 3.2 Graphical display of distributions

hist(x)
mid.age <- c(2.5,7.5,13,16.5,17.5,19,22.5,44.5,70.5)
acc.count <- c(28,46,58,20,31,64,149,316,103)
age.acc <- rep(mid.age,acc.count)
brk <- c(0,5,10,16,17,18,20,25,60,80)
hist(age.acc,breaks=brk)

n <- length(x)
plot(sort(x),(1:n)/n,type="s",ylim=c(0,1))
qqnorm(x)

data(IgM)
par(mfrow=c(1,2))
boxplot(IgM)
boxplot(log(IgM))
par(mfrow=c(1,1))

# 3.3 Summary statistics by group

data(red.cell.folate)
attach(red.cell.folate)
tapply(folate,ventilation,mean)
tapply(folate,ventilation,sd)
tapply(folate,ventilation,length)

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
cbind(mean=xbar, std.dev=s, n=n)

# juul data frame still attached
tapply(igf1, tanner, mean)
tapply(igf1, tanner, mean, na.rm=T)

# 3.4 Graphics for grouped data

data(energy)
attach(energy)
expend.lean <- expend[stature=="lean"]
expend.obese <- expend[stature=="obese"]
par(mfrow=c(2,1))
hist(expend.lean,breaks=10,xlim=c(5,13),ylim=c(0,4),col="white")
hist(expend.obese,breaks=10,xlim=c(5,13),ylim=c(0,4),col="grey")

par(mfrow=c(1,1))
boxplot(expend ~ stature)
boxplot(expend.lean,expend.obese)

opar <- par(mfrow=c(2,2),mex=0.8,mar=c(3,3,2,1)+.1)
stripchart(expend~stature)
stripchart(expend~stature,method="stack")
stripchart(expend~stature,method="jitter")
stripchart(expend~stature,"jitter",jitter=.03)
par(opar)

# 3.5 Tables

caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218
,327,106,67),
nrow=3,byrow=T)
caff.marital
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital

# using juul data frame, still attached
table(sex)
table(sex,menarche)
table(menarche,tanner)
t(caff.marital)
tanner.sex <- table(tanner,sex)
tanner.sex

margin.table(tanner.sex,1)
margin.table(tanner.sex,2)
prop.table(tanner.sex,1)
tanner.sex/sum(tanner.sex)

# 3.6 Graphical display of tables

total.caff <- margin.table(caff.marital,2) 
total.caff
barplot(total.caff, col="white")

par(mfrow=c(2,2))
barplot(caff.marital, col="white")
barplot(t(caff.marital), col="white")
barplot(t(caff.marital), col="white", beside=T)
barplot(prop.table(t(caff.marital),2), col="white", beside=T)
par(mfrow=c(1,1))

barplot(prop.table(t(caff.marital),2),beside=T, 
legend.text=colnames(caff.marital),
col=c("white","grey80","grey50","black"))
dotchart(t(caff.marital))

opar <- par(mfrow=c(2,2),mex=0.8, mar=c(1,1,2,1))
slices <- c("white","grey80","grey50","black")
pie(caff.marital["Married",], main="Married", col=slices)
pie(caff.marital["Prev.married",],
         main="Previously married", col=slices)
pie(caff.marital["Single",], main="Single", col=slices)
par(opar)
