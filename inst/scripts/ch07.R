# Chapter 7: Tabular data

# 7.1 Single proportions

prop.test(39,215,.15)
binom.test(39,215,.15)

# 7.2 Two independent proportions

lewitt.machin.success <- c(9,4)
lewitt.machin.total <- c(12,13)
prop.test(lewitt.machin.success,lewitt.machin.total)
matrix(c(9,4,3,9),2) 
lewitt.machin <- matrix(c(9,4,3,9),2)
fisher.test(lewitt.machin)
chisq.test(lewitt.machin)

# 7.3 k proportions, test for trend

caesar.shoe <- matrix(c(5,7,6,7,8,10,
                   17,28,36,41,46,140), nrow=2, byrow=T)
colnames(caesar.shoe) <- c("<4","4","4.5","5","5.5","6+")
rownames(caesar.shoe) <- c("Yes","No")
caesar.shoe
caesar.shoe.yes <- caesar.shoe["Yes",]
caesar.shoe.total <- margin.table(caesar.shoe,2)    
caesar.shoe.yes                     
caesar.shoe.total                          
prop.test(caesar.shoe.yes,caesar.shoe.total)
prop.trend.test(caesar.shoe.yes,caesar.shoe.total)

# 7.4 r x c tables

caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218
    ,327,106,67),
nrow=3,byrow=T)
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital
chisq.test(caff.marital)
chisq.test(caff.marital)$expected
chisq.test(caff.marital)$observed
E <- chisq.test(caff.marital)$expected   
O <- chisq.test(caff.marital)$observed   
(O-E)^2/E

data(juul)
attach(juul)
chisq.test(tanner,sex)            
