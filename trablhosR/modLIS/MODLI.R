# modelos para as propriedades isicas de soluções aquaosas com LI

# 0.set the dir and load packages
# 1. load datatables and first descritive analysis
# 2. Exploratory Analysis 
# 3. Scatter and box plots ans linearizations 
# 4. Linear Models 
# 5. Non-linear models 

#=================================================================
# 0. set the dir and load packages 

setwd("~/Dropbox/r")

library(ggplot2)
library(KRLS)
library(pls)
library(gtools)
library(lattice)
library(ggfortify)
library(caret)

#=================================================================
# 1.load the datatables 

sdtab <-function (tab,cv=F) {

	if (cv==F) {
	x <-apply(tab,2,sd)}
    else { x <-apply(tab,2,sd)/apply(tab,2,mean) }
    print(x)
    return(x)    
	}
#-------------------------------------
# for viscosity

tab1 <-read.delim("ILprop")
summary(tab1)
names(tab1)

attach(tab1)
tab11 <-subset(tab1,!Viscosity..cP.=="NA") 
tab11 <-subset(tab1,!Viscosity..cP.>200) 
detach(tab1)


summary(tab11)

plot(tab11[,7])

hist(tab11[,7],breaks=20,col="green",main= "Viscosity (cP)",xlab="")

#-------------------------------------
# for specific heat

tab2 <-read.delim("ILcp")
summary(tab2)
names(tab2)

hist(tab2[,2],breaks=30, col="blue",main= "Heat Capacity (kJ/mol.K)",xlab="")


#=================================================================
# 2. Exploratory Analysis 

#------------------------------------------------------------------
# for viscosity

attach(tab11)
tab11pca <-data.frame(massfractionIL,molarfractionIL,density,Viscosity..cP.,Temperature.K.,MW,Alkyl.Lenght,MVIL,Mwcation,Mwanion)
tab11pca <-na.omit(tab11pca)
detach(tab11)

pca1 <-prcomp(tab11pca,scale=T)
summary(pca1) 
plot(pca1)
loadings(pca1)

#biplots iniciais para avaliação
biplot(pca1)
biplot(pca1,choices=c(1,3))

#------------------------------------------------------------------
# for specific heat
attach(tab2)
tab2pca <-data.frame(Concentração,Cp,Temperatura,MM)
detach(tab2)
loadings(pca2)

pca2 <-prcomp(tab2pca,scale=T)
summary(pca2)
plot(pca2)

#biplots iniciais para avaliação
biplot(pca2)
biplot(pca2,choices=c(1,3))
#=================================================================
# 3. Scatter and box plots ans linearizations
 
#-------------------------------------
# for viscosity

cor(tab11pca)
attach(tab11pca)

#binary plots between viscosity and massfractionIL

tab11alt <-subset(tab11pca,!massfractionIL==0)

detach(tab11pca)
attach(tab11alt)

invVis2 = 1/Viscosity..cP.
logVisInv = exp(1/Viscosity..cP.)

#--------- Melhores ----------------#
plot(logVisInv~massfractionIL)
plot(invVis2~massfractionIL) 
#-----------------------------------# 
detach(tab11alt)
#binary plots between viscosity and alkyl lenght

#não tem correlação significativa
plot(Viscosity..cP.~Alkyl.Lenght)
boxplot(Viscosity..cP.~Alkyl.Lenght)

#binary plots between viscosity and MVIL

plot(Viscosity..cP.~MVIL)
expm1MVIL = exp(-MVIL)
expm1MVIL = exp(-1/MVIL)
invMVIL = 1/MVIL
plot(Viscosity..cP.~invMVIL)
plot(invVis2~invMVIL)

#binary plots between viscosity and Temperatura

plot(Viscosity..cP.~Temperature.K.)
boxplot(Viscosity..cP.~Temperature.K.)

#binary plots between viscosity and density

plot(Viscosity..cP.~density)
plot(invVis2~density)

#------------------------------------------------------------------
# for specific heat
cor(tab2pca)

attach(tab2pca)

plot(Cp~Concentração)
plot(Cp~Temperatura)
boxplot(Cp~Temperatura)
plot(Cp~MM)

boxplot(Cp~MM)

detach(tab2pca)


#=================================================================
# 4. linear models 

#-----------------------------------------------------------
#modelos lineares para viscosidades 

attach(tab11alt)
lm1 <-lm(logVisInv~massfractionIL)
summary(lm1)

lm2 <-lm(invVis2~massfractionIL) # melhor correlação 
summary(lm2)

lm3 <-lm(invVis2~density)
summary(lm3)

lm4 <-lm(invVis2~MVIL)
summary(lm4)

lm5 <-lm(invVis2~Alkyl.Lenght)
summary(lm5)

lm6 <-lm(invVis2~massfractionIL+density)
summary(lm6)

lm7 <-lm(massfractionIL~density)
summary(lm7)
#-----------------------------------------------------------------------
modtab = tab11alt
modtab = modtab[-65,]
invVis3 = 1/modtab[,4]
lm081 <-lm(invVis3~massfractionIL*density*MVIL,data=modtab) #melhor correlação 
summary(lm081)
predplot(lm081)
#-----------------------------------------------------------------------

lm082 <-lm(invVis2~massfractionIL+massfractionIL:density+MVIL:massfractionIL)
summary(lm082)

#-----------------------------------------------------------------------
lm083 <-lm(invVis3~massfractionIL+massfractionIL:density+MVIL:massfractionIL+MVIL:density,data=modtab) # melhor significância estátistica
summary(lm083)
predplot(lm083)

detach(tab11alt)

#-----------------------------------------------------------
#model for specific heat

attach(tab2pca)

lm8 <-lm(Cp~Concentração+Concentração:Temperatura+MM) # esse é o modelo melhor
summary(lm8)

detach(tab2pca)



