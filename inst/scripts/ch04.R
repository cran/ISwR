# Chapter 4: One- and two-sample tests

# 4.1 One-sample t test

daily.intake <- c(5260,5470,5640,6180,6390,6515,
6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)
t.test(daily.intake,mu=7725)

# 4.2 Wilcoxon signed-rank test

wilcox.test(daily.intake, mu=7725)

# 4.3 Two-sample t test

data(energy)
attach(energy)
energy
t.test(expend~stature)
t.test(expend~stature, var.equal=T)

# 4.4 Comparison of variances

var.test(expend~stature)

# 4.5 Two-sample Wilcoxon test

wilcox.test(expend~stature)

# 4.6 The paired t test

data(intake)
attach(intake)
intake
post - pre
t.test(pre, post, paired=T)
t.test(pre, post) #WRONG!

# 4.7 The matched-pairs Wilcoxon test

wilcox.test(pre, post, paired=T)
