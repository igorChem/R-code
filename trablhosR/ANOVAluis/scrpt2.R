# Script R

# 0. First things
# 1. Loading data
# 2. anova 

#=======================================================================

# 0. First things

# setting working directory

setwd("~/")
setwd("~/Dropbox/r/ANOVAluis")

# setting libraries path

.libPaths("~/R/x86_64-pc-linux-gnu-library/3.0")

#=======================================================================

# 1. loading data 

tab1 <-read.table("Anova",header=T) 
attach(tab1)
str(tab1)
names(tab1)

#-----------------------------------------------------------------------
[1] "tempo"  "MMPBSA" "MMGBSA"
#-----------------------------------------------------------------------

MMPBSA.F <-rep("MMPBSA",7)
MMGBSA.F <-rep("MMGBSA",7)

fator  <-c(MMPBSA.F,MMGBSA.F)
energy <-c(MMPBSA,MMGBSA)
Tempo  <-rep(tempo,2)

tab1df <-data.frame(Tempo,fator,energy)


#=======================================================================

# 2. Modelos 

# one-way ANOVA 

fit1 <-aov(energy~fator)
summary(fit1)
coefficients(fit1)

# Análises de ANCOVA 

#=======================================================================

fit2 <-aov(energy~fator+Tempo)
summary(fit2)
coefficients(fit2)


fit3 <-aov(energy~fator*Tempo)
summary(fit3)
coefficients(fit3)

ancova(energy~fator+Tempo)

tiff("boxplot.tiff",res=200,units="in",height=6,width=10)
par(mfrow=c(1,2))
boxplot(energy~fator,ylab="Energia")
boxplot(energy~Tempo,ylab="Energia",xlab="Tempo")
dev.off()

tab2 <-subset(tab1df,fator=="MMPBSA")
tab2
tab3 <-subset(tab1df,fator=="MMGBSA")
tab3

#=======================================================================

aggregate(energy~fator,data=tab1,mean)

aggregate(energy~fator,data=tab1,sd)

