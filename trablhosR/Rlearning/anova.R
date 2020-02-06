#Anova chapter of Rbook 

# aov() function are quite simmilar with lm() function, but displays in a more
# familiar way with the anova methodologies

# aov uses "formula" form, the response variable is put in function of the factors
# the ":" means interactions between the factors
# 

# one way anova exercises 
attach(cholesterol)
table(trt) # group sample sizes 

# table function table uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.

aggregate(rr, by=list(trt),FUN=mean, data=cholesterol)
aggregate(rr,by=list(trt),FUN=sd)


#test for group differences 

fit <-aov(rr~trt)
summary(fit)
plotmeans(rr~trt) # cool tool for r plot in anova 
TukeyHSD(fit) # calculate pairwise differences for between groups 

par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))
