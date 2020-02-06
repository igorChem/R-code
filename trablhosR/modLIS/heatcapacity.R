# R script for Heat capacitiy moelling of ionic liquids in water 

# 0. Load packages and functions 
# 1. Load data and summary tables
# 2. binary plots
# 3. Linear models 

#=======================================================================

# 0. Load packages and functions 

setwd("~/Dropbox/r")

library(pls)

sdtab <- function (tab,cv=F) {

	if (cv==F) {
	x <-apply(tab,2,sd)}
    else { 
    x <-apply(tab,2,sd)/apply(tab,2,mean) 
    }
    print(x)    
	}
	
#=======================================================================
# 1. Load data and summary tables

tab2 <-read.delim("ILcp")
summary(tab2)
names(tab2)

sdtab(tab2)
attach(tab2)
tab22 <-data.frame(Concentração,Cp,MM,Temperatura)
cor(tab22)

tiff("dsitribuicaoHeatCp",width = 7, height = 7, units = 'in', res = 225)

par(mfrow=c(2,2))
hist(tab2[,2],breaks=30, col="blue",main= "Capacidade Calorifica (kJ/mol.K)",xlab="",ylab="Frequencia")
hist(tab2[,3],breaks=30, col="red",main= "Temperatura (K)" ,xlab="",ylab="Frequencia")
hist(tab2[,1],breaks=30, col="green",main= "Fracao Massica de LI (%)",xlab="",ylab="Frequencia")
hist(tab2[,5],breaks=30, col="yellow",main= "Massa molar do LI (ml/mol)",xlab="",ylab="Frequencia")

dev.off()
#=======================================================================
# 2. binary plots

attach(tab2)

Cp = tab2[,2]

tiff("binaryHeatCp",width = 7, height = 7, units = 'in', res = 225)

par(mfrow=c(2,2))
plot(Cp~Concentração,col="blue",ylab="Capacidade Calorifica (kJ/mol.K)",xlab="Fracao Massica de LI (%)")
plot(Cp~MM, col="green",ylab="Capacidade Calorifica (kJ/mol.K)",xlab="Massa molar do LI (ml/mol)")
plot(Cp~Temperatura,col="red",ylab="Capacidade Calorifica (kJ/mol.K)",xlab="Temperatura (K)")
boxplot(Cp~Temperatura,col="red",ylab="Capacidade Calorifica (kJ/mol.K)",xlab="Temperatura (K)")

dev.off()

newvar = Concentração*Temperatura

par(mfrow=c(1,1))
plot(Cp~newvar)


#=======================================================================# 
# 3. Linear models 

set.seed(1701)

tab00 <-data.frame(Cp,Concentração,Temperatura,MM)

detach(tab2)
attach(tab00)

smp_size <- floor(0.8 * nrow(tab00))

samp <-sample(seq_len(nrow(tab00)), size =smp_size)
train <-tab00[samp, ]
test  <-tab00[-samp, ]


lm8 <-lm(Cp~Concentração+Concentração:Temperatura+MM,data=train) 
summary(lm8)

#***********************************************************************
#Call:
#lm(formula = Cp ~ Concentração + Concentração:Temperatura + 
#    MM, data = train)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-45.372  -4.468   1.763   5.368  35.442 
#
#Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               342.39311    3.88594  88.111  < 2e-16 ***
#Concentração             -339.48804   17.93884 -18.925  < 2e-16 ***
#MM                          0.04898    0.01183   4.138 5.58e-05 ***
#Concentração:Temperatura    0.19651    0.05649   3.479 0.000645 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 10.81 on 164 degrees of freedom
#Multiple R-squared:  0.9872,	Adjusted R-squared:  0.987 
#F-statistic:  4224 on 3 and 164 DF,  p-value: < 2.2e-16
#**********************************************************************

predicted02 <-predict.lm(lm8,test)


tiff("predplotHEATcp",width = 8, height = 8, units = 'in', res = 225)

plot(fitted(lm8)~train[,1],col="blue",ylab="Capacidade Calorifica Calculada",
            xlab="Capacidade Calorifica  Experimental",main="")
            points(test[,1],predicted02,col="red")
            legend("bottomright",legend=c("Dados de Teste","Dados do modelo"),
            pch=c(1,1),lwd=c(2,2),col=c("red","blue"),cex=.8)
            abline(lm(fitted(lm8)~train[,1]),col="green")
            
dev.off()
