# Chapter 12: Survival analysis

# 12.2 Survival objects

library(survival)
data(melanom)
attach(melanom)
names(melanom)
Surv(days, status==1)

# 12.3 Kaplan-Meier estimates

survfit(Surv(days,status==1)) 
surv.all <- survfit(Surv(days,status==1))
summary(surv.all)
plot(surv.all)
surv.bysex <- survfit(Surv(days,status==1)~sex)
plot(surv.bysex)
plot(surv.bysex, conf.int=T, col=c("black","gray"))

# 12.4 The log-rank test

survdiff(Surv(days,status==1)~sex)
survdiff(Surv(days,status==1)~sex+strata(ulc))

# 12.5 The Cox proportional hazards model

summary(coxph(Surv(days,status==1)~sex))
summary(coxph(Surv(days,status==1)~sex+log(thick)+strata(ulc)))
plot(survfit(coxph(Surv(days,status==1)~
             log(thick)+sex+strata(ulc))))
