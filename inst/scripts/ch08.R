# Chapter 8: Power and the computation of sample size

# 8.1 The principles of power calculations

curve(pt(x,25,ncp=3), from=0, to=6)
abline(v=qt(.975,25))
pt(qt(.975,25),25,ncp=3)

# 8.2 Two-sample problems

power.t.test(delta=0.5, sd=2, sig.level = 0.01, power=0.9)
power.t.test(n=450, delta=0.5, sd=2, sig.level = 0.01)
power.t.test(delta=0.5, sd=2, sig.level = 0.01, power=0.9,
alt="one.sided")

# 8.3 One-sample problems and paired tests

power.t.test(delta=10, sd=10*sqrt(2), power=0.85, type="paired")

# 8.4 Comparison of proportions

power.prop.test(power=.85,p1=.15,p2=.30)
