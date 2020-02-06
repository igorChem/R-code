# Análises IL features for CO2 capture
# congresso suiça

# 0. first adjusts 
# 1. load data
# 2. exploratpry analysis
# 3. Modelos ANOVA 
# 4. PCA 

#===========================================================

# 0. load packges

#.libPaths("~/R/x86_64-pc-linux-gnu-library/3.0")

# library(caret)

library(pls)
library(gtools)
library(lattice)
library(ggfortify)

# set dir

setwd("~/Dropbox/r/GHGT")

setwd("~/igor/Dropbox/Dropbox/r/GHGT")

#===========================================================

# 1. Load data ---------------------------------------------

ILtable <- read.delim("ILtable", row.names=1)
#View(ILtable)
names(ILtable)
rn.Cation <-c("EMI","C4MI","EMI","C3MI","C8MI","C4MI","C3MI","EMI","BDMI","C4MI","N6111","M4B-py","P14","C3MI","C3MI","C6MI","C4MI","C4-py","C4-py","C4-py","C10MI","C8MI","EMI","EMI","N1114","C4MI")


anova <-read.delim("anova",row.names=1)
attach(anova)

HCanova <-subset(anova,!HC=="NA")
DTanova <-subset(anova,!DT=="NA")
MPanova <-subset(anova,!MP=="NA")
VisAnova <- subset(anova,!viscosidade=="NA")

detach(anova)

PCA <-read.delim("PCA",row.names=1)

#===========================================================

# 2. exploratpry analysis-----------------------------------

# anova tables 

attach(HCanova)

tiff("HCanion.tiff",res=200,width=10,height=6,units="in")
par(mfrow=c(1,2))
plot(HC~Anion,ylab="Henry Constant")
plot(HC~Cation,ylab="Henry Constant")
dev.off()

tiff("HCcation.tiff",res=200,width=6,height=6,units="in")
plot(HC~Cation,ylab="Henry Constant")
dev.off()

fit1 <-aov(HC~Anion)
summary(fit1)
fit2 <-aov(HC~Cation)
summary(fit2)
fit3 <-aov(HC~Anion+Cation)
summary(fit3)
coefficients(fit3)
fit4 <-aov(HC~Anion*Cation)
summary(fit4)

lm1 <-lm(HC~Anion+Cation)
summary(lm1)

plot(HC~MM)
plot(HC~nCarbonosLaterais)


detach(HCanova)

#---------------------------------------------------------
attach(DTanova) 


tiff("DTanion.tiff",res=200,width=10,height=6,units="in")
par(mfrow=c(1,2))
plot(DT~Anion,ylab="Degradation Temperature (K)")
plot(DT~Cation,ylab="Degradation Temperature (K)")
dev.off()


fit5 <-aov(DT~Anion+Cation)
summary(fit5)
lm2 <-lm(DT~Anion+Cation)
summary(lm2)

detach(DTanova)

# ---------------------------------------------------------------------

attach(MPanova)

tiff("MPanion.tiff",res=200,width=10,height=6,units="in")
par(mfrow=c(1,2))
plot(MP~Anion,ylab="Melting Temperature (K)")
plot(MP~Cation,ylab="Melting Temperature (K)")
dev.off()

tiff("MPcation.tiff",res=200,width=6,height=6,units="in")
plot(MP~Cation,ylab="Melting Temperature (K)")
dev.off()


fit7 <-aov(MP~Anion+Cation)
summary(fit7)
lm3 <-lm(MP~Anion+Cation)
summary(lm3)

fit8 <-aov(MP~Anion:Cation)
summary(fit8)
lm4 <-lm(MP~Anion:Cation)
summary(lm4)

fit9 <-aov(MP~Cation)
summary(fit9)
lm5 <-lm(MP~Cation)
summary(lm5)

detach(MPanova)

# viscotity-----------------------------------------------------------

VisAnova <-VisAnova[-15,]
attach(VisAnova)


tiff("Visanion.tiff",res=200,width=10,height=6,units="in")
par(mfrow=c(1,2))
plot(viscosidade~Anion,ylab="Viscosity (cp)")
plot(viscosidade~Cation,ylab="Viscosity (cp")
dev.off()



fit10 <-aov(viscosidade~Cation+Anion)
summary(fit10)
lm6 <-lm(log(viscosidade)~Cation+Anion)
summary(lm6)

detach(VisAnova)

# 4. PCA------------------------------------------------------ 

##PCA tabelas------------------------------------------------

attach(PCA)

tab1 <-PCA[,-8];tab1
tab1 <-tab1[,-6];tab1
tab2 <-na.omit(tab1[,1:7])

anion.group <-c("NCN2","BF4","BF4","PF6","BF4","PF6","BF4",
"Tfo","Tfo","TFSI","BF4","TFSI","TFSI","BF4","TFSI","TFSI","TFSI","PF6",
"TFSI","NCN2")

cation.group <-c("MIa","C4MI","MIa","MIa","MIb","C4MI","MIa","MIa","C4MI",
"N4","py","N4","C4MI","py","py","MIb","MIa","MIb","N4","C4MI")

tab3 <-data.frame(tab2,anion.group,cation.group)
pca1 <-prcomp(tab3[1:7],scale=T)
summary(pca1)

tiff("pcaIplot.tiff",units="in",width=6,height=5,res=220)
autoplot(pca1,xlab="PC1(50%)",ylab="PC2(27%)",loadings=T,loadings.label = TRUE)
dev.off()

tiff("pcaIplotloadings.tiff",units="in",width=6,height=5,res=220)
autoplot(pca1,xlab="PC1(50%)",ylab="PC3(27%)",
loadings=F,colour="anion.group",data=tab3,frame=T)
dev.off()

tiff("pcaIplotcation.tiff",units="in",width=6,height=5,res=220)
autoplot(pca1,xlab="PC1(50%)",ylab="PC3(27%)",
loadings=F,colour="cation.group",data=tab3,frame=T)
dev.off()


#------------------------------------------------------------


tab3 <-data.frame(tab3,rn.pca1)
pca3 <-prcomp(tab3)
summary(pca3)
loadings(pca3)
biplot(pca3) #meh

#--------------------------------------------------------

tab4 <-tab3[,1:8]
pca4 <-prcomp(tab4)
pca5 <-prcomp(tab4,scale=T)

summary(pca4)
loadings(pca4)
biplot(pca4)

summary(pca5)
loadings(pca5)
biplot(pca5) # boa 



autoplot(pca5,xlab="PC1(50%)",ylab="PC2(27%)",loadings=T,loadings.label = TRUE)
autoplot(pca5,choices=c(1,3),xlab="PC1(50%)",ylab="PC3(10%)",
loadings=T,loadings.label = TRUE,colour="rn.pca1",data=tab3,frame=T)

df <-tab3[c(1:8)]
pc6 <-prcomp(df,scale=T)

autoplot(pca5,choices=c(1,3),xlab="PC1(50%)",ylab="PC3(10%)",
loadings=T,loadings.label = TRUE,colour="rn.pca1",data=tab3)
#-------------------------------------------------------
# novos plots de exploração

xyplot(DT~MP|Cation)# nada
xyplot(DT~MP|Anion)
xyplot
tab3 <-data.frame(MM,nCarbonosLaterais,VolumeMolar,densidade,viscosidade,Anion,Cation)
tab3 <-na.omit(tab3)
tab3 <-tab3[-20,](DT~nCarbonosLaterais|Cation) # boa 
xyplot(DT~nCarbonosLaterais|Anion)
xyplot(DT~MM|Cation) # boa
xyplot(DT~MM|Anion) # boa 

## PCA com viscosidade e HC-------------------------------

tab1 <-PCA

# rm Na's from viscosidade, usar HC e os outros inteiros 

## PCA com DT

tab5 <-subset(PCA,!DT=="NA")
tab05 <-tab5[,1:4]
pca6 <-factanal(DT~tab05)
## PCA com MP 


# plots 
plot(HC~MM) # LIs de maior massa molar absorvem mais 
xyplot(HC~MM|Cation)
xyplot(HC~MM|Anion,type=c("smooth")) # para BF4 e PF6 é muito forte esse trend
plot(log(HC)~log(MM)) #ta bom pra fazer modelo 

plot(HC~densidade)
xyplot(log(HC)~densidade|Cation) # correlações interessantes
library(pls)

xyplot(HC~VolumeMolar|Anion)
xyplot(HC~VolumeMolar|Cation)    
xyplot(HC~nCarbonosLaterais|Anion,groups = Cation) # não faz sentido

xyplot(HC~MP|Cation)
xyplot(HC~MP|Anion)

ps1 <-mvr(log(HC)~densidade*MM*VolumeMolar,3,validation = "CV",method = "oscorespls",scale = T) 
summary(ps1)

tab7 <-PCA[-24,]
pls2 <-mvr(viscosidade~nCarbonosLaterais*MM*MP,data=tab7,method = "oscorespls",scale = T,validation = "CV")
summary(pls2)

#---------------------------------------------------------------------------------

xyplot(viscosidade~nCarbonosLaterais|Anion,data = tab7)# trabalhar na viscosidade
xyplot(viscosidade~MP|Anion,data = tab7)
xyplot(MP~nCarbonosLaterais)
xyplot(MP~VolumeMolar|Cation)
xyplot(MP~MM|Cation)

## =========================================plotting PCA in R ggfortify============================

attach(ILtable)

labs <-row.names(ILtable)

tab3 <-data.frame(row.names=labs,MM,nCarbonosLaterais,VolumeMolar,densidade,viscosidade)
tab3 <-na.omit(tab3)
tab3 <-tab3[-20,]

pca1 <-prcomp(tab3[,1:5],scale=T)

autoplot(pca1,loadings=T,loadings.label=T,label=T)

plot(tab3[,5]~tab3[,4])
plot(tab3[,5]~tab3[,3])
plot(tab3[,5]~tab3[,2])
plot(tab3[,5]~tab3[,1])

lm7 <-lm(viscosidade[-20]~densidade[-20])

tab4 <-data.frame(MM,nCarbonosLaterais,VolumeMolar,densidade,MP)
tab4 <-na.omit(tab4)

pca2 <-prcomp(tab4,scale=T)
summary(pca2)
biplot(pca2)

plot(MP~nCarbonosLaterais)
plot(MP~densidade)



# PCA analysis


