# Chapter 6: ANOVA and Kruskal-Wallis

# 6.1 One-way analysis of variance

data(red.cell.folate)
attach(red.cell.folate)
summary(red.cell.folate)
anova(lm(folate~ventilation))

data(juul)
attach(juul)
anova(lm(igf1~tanner))  ## WRONG!
juul$tanner <- factor(juul$tanner, 
                      labels=c("I","II","III","IV","V"))
detach(juul)
attach(juul)
summary(tanner)
anova(lm(igf1~tanner))

summary(lm(folate~ventilation))

pairwise.t.test(folate, ventilation, p.adj="bonferroni")
pairwise.t.test(folate,ventilation)
oneway.test(folate~ventilation)
pairwise.t.test(folate,ventilation,pool.sd=F)

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
sem <- s/sqrt(n)
stripchart(folate~ventilation,"jitter",jit=0.05,pch=16,vert=T)
arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
lines(1:3,xbar,pch=4,type="b",cex=2)

bartlett.test(folate~ventilation)

# 6.2 Kruskal-Wallis test

kruskal.test(folate~ventilation)

# 6.3 Two-way analysis of variance

data(heart.rate)
attach(heart.rate)
heart.rate
gl(9,1,36)
gl(4,9,36,labels=c(0,30,60,120)) 
anova(lm(hr~subj+time))

interaction.plot(time, subj, hr)
interaction.plot(ordered(time),subj,hr)

# 6.4 The Friedman test

friedman.test(hr~time|subj,data=heart.rate)

# 6.5 The ANOVA table in regression analysis

data(thuesen)
attach(thuesen)
lm.velo <- lm(short.velocity~blood.glucose)
anova(lm.velo)
