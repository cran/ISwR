# Chapter 9: Multiple regression

# 9.1 Plotting multivariate data

data(cystfibr)
par(mex=0.5)
pairs(cystfibr, gap=0, cex.labels=0.9)
attach(cystfibr)
# you may need to rm(height, weight)

# 9.2 Model specification and output

summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))
1-25.5^2/var(pemax)
anova(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))
955.4+155.0+632.3+2862.2+1549.1+561.9+194.6+92.4 
7002.9/8
875.36/648.7
1-pf(1.349407,8,15)

m1<-lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)
m2<-lm(pemax~age)
anova(m1,m2)

# 9.3 Model search

summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))
summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc))
summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv))
summary(lm(pemax~age+sex+height+weight+bmp+fev1))
summary(lm(pemax~age+sex+height+weight+bmp))
summary(lm(pemax~age+height+weight+bmp))
summary(lm(pemax~height+weight+bmp))
summary(lm(pemax~weight+bmp))
summary(lm(pemax~weight))

summary(lm(pemax~age+weight+height))
summary(lm(pemax~age+height))
summary(lm(pemax~age))
summary(lm(pemax~height))
