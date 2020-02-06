# análises estatísticas dos cálculos de absorção

attach(error)
summary(error)
aggregate(nmolsCO2,by=list(Tipo), FUN=mean)

#Group.1            x
#1 BmimSLES_30% -0.004967310
#2  BmimSLS_30% -0.004968634
#3  BmimSLS_50% -0.004678556
#4          H2O -0.005422316
#6    MixILMDEA  0.002591426
#7     N4444BF4 -0.007558216
#8     P4444BF4  0.003473620


aggregate(nmolsCO2,by=list(Tipo), FUN=sd)

#Group.1           x
#1 BmimSLES_30% 0.004366064
#2  BmimSLS_30% 0.003882912
#3  BmimSLS_50% 0.004055807
#4          H2O 0.002809894
#5    MixILMDEA 0.006967844
#6     N4444BF4 0.002020067
#7     P4444BF4 0.017975536

aggregate(QP,by=list(Tipo), FUN=mean)

aggregate(QP,by=list(Tipo), FUN=sd)

#1 BmimSLES_30% 0.58010333
#2  BmimSLS_30% 0.53767693
#3  BmimSLS_50% 0.57303226
#4          H2O 0.42746437
#5    MixILMDEA 1.11831359
#6     N4444BF4 0.14849242
#7     P4444BF4 0.04949747

aggregate(Densidade_.l..quido,by=list(Tipo), FUN=sd)

#Group.1          x
#1 BmimSLES_30% 30.27181185
#2  BmimSLS_30% 0.28588327
#3  BmimSLS_50% 0.29988399
#4          H2O 0.09387715
#5    MixILMDEA 0.59909102
#6     N4444BF4 0.42178919
#7     P4444BF4 0.07615540


vol <- Massa_l..quido/Densidade_.l..quido

aggregate(vol,by=list(Tipo), FUN=sd)

aggregate(vol,by=list(Tipo), FUN=mean)
plotmeans(QP~Tipo)
plotmeans(nmolsCO2~Tipo)

fit1 <-aov(QP~Tipo)
TukeyHSD(fit1)
plot(TukeyHSD(fit1))

fit2 <-aov(nmolsCO2~Tipo)
TukeyHSD(fit2)

fit3 <-aov(vol~Tipo)
vol
TukeyHSD(fit3)

fit4 <-lm(nmolsCO2~QP)
summary(fit4)

xyplot(QP~vol|Tipo)
xyplot(nmolsCO2~vol|Tipo)
xyplot(QP~nmolsCO2|Tipo,cex=1.1,pch=5,type=c("g","p"))
xyplot(QP~nmolsCO2)
plot(QP,nmolsCO2,xlab = "Queda de Pressão",ylab = "nmols CO2absorvidos",
     main = "Celula de Equilibrio")
par(tck=1,cex=1.4)
abline(h=0,v=7.6,col="red")
xyplot(log(QP)~Densidade_.l..quido|Tipo)

