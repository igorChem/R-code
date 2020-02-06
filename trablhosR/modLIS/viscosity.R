# R scritp for viscosity modelling of ionic liquids in water 

# 0. Load packages and functions 
# 1. Load data and summary tables
# 2. Descritive statistcs and distribution plots 
# 3. Binary plots
# 4. Linear models 

#=======================================================================

# 0. Load packages and functions 

setwd("~/Dropbox/r")

library(pls)

#-----------------------------------------------------------------------

sdtab <- function (tab,cv=F) {

	if (cv==F) {
	x <-apply(tab,2,sd)}
    else { x <-apply(tab,2,sd)/apply(tab,2,mean) }
    print(x)
    return(x)    
	}
	
#=======================================================================
	
# 1. Load data and summary tables


tab1 <-read.delim("ILprop")
summary(tab1)
names(tab1)

attach(tab1)
tab11 <-subset(tab1,!Viscosity..cP.=="NA") 
tab11 <-subset(tab11,!Viscosity..cP.>200) 
tab11 <-subset(tab11,!massfractionIL==0)
detach(tab1)

tab11 <-tab11[-65,]

#=======================================================================

# 2. Descritive statistcs and distribution plots 

# summary tables 
summary(tab11)

# standard deviations 
sdtab(tab11)

attach(tab11)

cortab <-data.frame(massfractionIL,density,Viscosity..cP.,Temperature.K.,MVIL)
cor(cortab)

#-----------------------------------------------------------------------
# distribution plots 
 
tiff("dsitrubution_Viscosity",width = 6, height = 7, units = 'in', res = 225)

par(mfrow=c(2,2))

hist(Viscosity..cP.,breaks=20,col="green",main= "Viscosidade \n(cP)",xlab="",ylab="Frequência")
hist(massfractionIL,breaks=20,col="blue",main= "Líquido Iônico \n (%mássica)",xlab="",ylab="Frequência")
hist(density,breaks=20,col="red",main= "Densidade \n (g/ml)",xlab="",ylab="Frequência")
hist(MVIL,col="yellow",main= "Volume molar \n (ml/mol)",xlab="",ylab="Frequência")

dev.off()

#=======================================================================

# 3. Binary plots 

visc <-Viscosity..cP.
invVis <-1/Viscosity..cP.

#----------------------------------------------------------------------------------------------------
tiff("viscxvar.tiff",width = 6, height = 6.5, units = 'in', res = 225)

par(mfrow=c(2,2))
plot(visc~massfractionIL,xlab="Fração Mássica de IL(%)",ylab="Viscosidade (cP)",main="",col="red")
plot(invVis~massfractionIL,xlab="Fração Mássica de IL(%)",ylab="1/Viscosidade (1/cP)",main="",col="blue")
plot(visc~MVIL,xlab="Volume Molar (ml/mol)",ylab="Viscosidade (cP)",main="",col="red")
plot(invVis~MVIL,xlab="Volume Molar (ml/mol)",ylab="1/Viscosidade (1/cP)",main="",col="blue")
dev.off()
#-------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------

tiff("viscXdensity",width = 8, height = 5, units = 'in', res = 225)

par(mfrow=c(1,2))

plot(visc~density,xlab="Densidade (g/ml)",ylab="Viscosidade (cP)",main="",col="red")
plot(invVis~density,xlab="Densidade (g/ml)",ylab="1/Viscosidade (1/cP)",main="",col="blue")

dev.off()

#-------------------------------------------------------------------------------------------
tiff("viscXtempXalkyllength",width = 8, height = 8, units = 'in', res = 225)

par(mfrow=c(2,2))
plot(visc~Temperature.K.,xlab="Temperatura (K)",ylab="Viscosidade (cP)",main="Viscosidade (cP)",col="red")
plot(visc~Alkyl.Lenght,xlab="Comprimento da \n cadeia lateral",ylab="Viscosidade (cP)",main="Viscosidade (cP)",col="blue")
boxplot(visc~Temperature.K.,xlab="Temperatura (K)",ylab="Viscosidade (cP)",main="",col="red")
boxplot(visc~Alkyl.Lenght,xlab="Comprimento da \n cadeia lateral",ylab="Viscosidade (cP)",main="",col="blue")

dev.off()

#=======================================================================

# 4. Linear models 

#-------------------------
# preparing data tables 
#-------------------------

set.seed(1001) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 80% of data as sample from total 'n' rows of the data  

mfinv <- 1/massfractionIL

tab00 <-data.frame(massfractionIL,invVis,mfinv,density,MVIL)

smp_size <- floor(0.8 * nrow(tab00))

samp <-sample(seq_len(nrow(tab00)), size =smp_size)
train <-tab00[samp, ]
test  <-tab00[-samp, ]


lm01<-lm(invVis~massfractionIL+density+massfractionIL:MVIL+massfractionIL:density,data=train)  
summary(lm01)

#=======================================================================
#Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 -11.767114   1.901682  -6.188 1.38e-08 ***
#massfractionIL                3.025583   3.206249   0.944  0.34764    
#density                      12.937980   1.841729   7.025 2.75e-10 ***
#MVIL                          0.075899   0.009362   8.107 1.41e-12 ***
#massfractionIL:density       -9.338445   3.088813  -3.023  0.00318 ** 
#massfractionIL:MVIL          -0.042196   0.015284  -2.761  0.00687 ** 
#density:MVIL                 -0.076170   0.009051  -8.416 3.06e-13 ***
#massfractionIL:density:MVIL   0.064509   0.014576   4.426 2.47e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.0358 on 99 degrees of freedom
#Multiple R-squared:  0.9872,	Adjusted R-squared:  0.9862 
#F-statistic:  1087 on 7 and 99 DF,  p-value: < 2.2e-16

#=======================================================================

predicted01 <-predict(lm01,test)
summary(predicted01)

tiff("predplotlm10",width = 8, height = 8, units = 'in', res = 225)

plot(fitted(lm01)~train[,2],col="blue",ylab="1/Viscosidade  Calculada",
            xlab="1/Viscosdade Experimental",main="")
            points(test[,2],predicted01,col="red")
            legend("bottomright",legend=c("Dados de Teste","Dados do modelo"),
            pch=c(1,1),lwd=c(2,2),col=c("red","blue"),cex=.8)
            abline(lm(fitted(lm01)~train[,2]),col="green")
            
dev.off()


#-----------------------------------------------------------------------

