# Chapter 5: Regression and correlation

# 5.1 Simple linear regression

data(thuesen)
attach(thuesen)
lm(short.velocity~blood.glucose)
summary(lm(short.velocity~blood.glucose))
plot(blood.glucose,short.velocity)
abline(lm(short.velocity~blood.glucose))

# 5.2 Residuals and fitted values

lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)
resid(lm.velo)

plot(blood.glucose,short.velocity)
lines(blood.glucose,fitted(lm.velo)) # throws error
lines(blood.glucose[!is.na(short.velocity)],fitted(lm.velo))

options(na.action=na.exclude)
lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)
segments(blood.glucose,fitted(lm.velo),
         blood.glucose,short.velocity)
plot(fitted(lm.velo),resid(lm.velo))
qqnorm(resid(lm.velo))

# 5.3 Prediction and confidence bands

predict(lm.velo)
predict(lm.velo,int="c")
predict(lm.velo,int="p")
pred.frame <- data.frame(blood.glucose=4:20)
pp <- predict(lm.velo, int="p", newdata=pred.frame)
pc <- predict(lm.velo, int="c", newdata=pred.frame)
plot(blood.glucose,short.velocity,
     ylim=range(short.velocity, pp, na.rm=T))
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc, pc, lty=c(1,2,2), col="black")
matlines(pred.gluc, pp, lty=c(1,3,3), col="black")

# 5.4 Correlation

cor(blood.glucose,short.velocity) # error
cor(blood.glucose,short.velocity,use="complete.obs")
cor(thuesen,use="complete.obs")

cor.test(blood.glucose,short.velocity)
cor.test(blood.glucose,short.velocity,method="spearman")
cor.test(blood.glucose,short.velocity,method="kendall")
