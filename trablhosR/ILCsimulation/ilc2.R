# análises estatísticas das simulações computacionais 


# 0. Packages and otheres alilke 
# 1. Matrix reading, loading and creation 
# 2. plots e linearizações 
# 3. Exploratory data analysis  
# 4. Simulations for feature selection 
# 5. Models 

#=======================================================================

# 0. packages and directories ------------------------------------------

# load packages---------------------------------------------------------

#.libPaths("/home/igor/R/x86_64-pc-linux-gnu-library/3.0")

library(caret)
library(pls)
library(kernlab)
library(KRLS)

# Set directories-------------------------------------------------------

setwd("~")
setwd("Dropbox/r/ILCsimulation")

#=======================================================================

# 1.read archives ------------------------------------------------------

ilc <-read.delim("ilc")
# removing unbalanced objects 

ilc1 <- ilc
ilc <- subset(ilc,tempo=="12")
ilc <-ilc[,-2]
ilc <-ilc[,-2]
ilc <-ilc[-6,]

str(ilc)
names(ilc)

#-----------------------------------------------------------------------
 [1] "IL"                 "Conv"               "Selc"              
 [4] "Yield"              "Homo"               "Lumo"              
 [7] "Fukui_Eleto"        "Fukui_Nucleo"       "Hardness"          
[10] "Softness"           "Eletronegativity"   "Acidity"           
[13] "Basicity"           "Chemical_Potential" "DeltaN1"           
[16] "DeltaN2"            "Cation"             "Anion"

#=======================================================================

# 2. plots e linearizações----------------------------------------------


attach(ilc)

boxplot(Yield)
boxplot(Selectivity)
boxplot(Conv)

par(mfrow=c(1,3))
hist(Yield)
hist(Selectivity)
hist(Conv)

# criação de variáveis--------------------------------------------------

lsoftN <- (Softness*Fukui_Nucleo)
lsoftE <- (Softness*Fukui_Eleto)

# plots para selectivity -----------------------------------------------

# estudos para temperatura ---------------------------------------------

attach(ilc1)
str(ilc1)

ilc2 <-subset(ilc1,IL=="bmimcl")
ilc3 <-subset(ilc1,IL=="bmimbf4")

attach(ilc2)
par(mfrow=c(1,3))
plot(Selectivity~tempo)
plot(Conv~tempo)
plot(Yield~tempo)

par(mfrow=c(1,3))
plot(Selectivity~temp)
plot(Conv~temp)
plot(Yield~temp)

detach(ilc2)
attach(ilc3)
plot(Selectivity~tempo)
plot(Conv~tempo)
plot(Yield~tempo)

plot(Selectivity~temp)
plot(Conv~temp)
plot(Yield~temp)

#=======================================================================

attach(ilc)

basicity1 <-(Chemical_Potential/Hardness)
acidity1 <-basicity1

plot(Selectivity~Cation)
plot(Selectivity~Anion)
plot(basicity1~Cation)
plot(basicity1~Anion)

plot(Selectivity~Cation)
plot(Selectivity~Anion)
plot(acidity1~Cation)
plot(acidity1~Anion)

tiff("plot1.tiff",units="in",res=220,height=9,width=6)
par(mfrow=c(2,2))
plot(Selectivity~Cation)
plot(Selectivity~Anion)
plot(Hardness~Cation)
plot(Hardness~Anion)
plot(Acidity~Cation)
plot(Acidity~Anion)
plot(Basicity~Cation)
plot(Basicity~Anion)
dev.off()



tiff("plot2.tiff",units="in",res=220,height=8,width=6.2)
par(mfrow=c(3,2))
plot(Yield~Cation)
plot(Yield~Anion)
plot(Conv~Cation)
plot(Conv~Anion)
plot(Fukui_Eleto~Cation)
plot(Fukui_Eleto~Anion)
dev.off()

#=======================================================================

# 3. Exploratory data analysis

# pca-------------------------------------------------------------------
rownames(ilc)<-ilc[,1] 
pca1 <-prcomp(ilc[,2:16],scale=T)
summary(pca1)
biplot(pca1)

attach(ilc)
df.pc2 <-data.frame(Selectivity,Hardness,Yield,
Conversion=Conv,Acidity,Softness,DeltaN=DeltaN1,Fukui_E=Fukui_Eleto,Basicity)

rownames(df.pc2)<-ilc[,1] 
pca2 <-prcomp(df.pc2,scale=T)
summary(pca2)
biplot(pca2,xlim=c(-0.45,0.8))

tiff("plotpca1.tiff",units="in",height=6,width=6,res=220)
biplot(pca2,xlim=c(-0.45,0.85),xlab="PC1 (54%)",ylab="PC2 (25%)")
dev.off()


tiff("plotpca2.tiff",units="in",height=6,width=6,res=220)
	biplot(pca2,xlim=c(-0.5,0.85),ylim=c(-0.45,0.4),xlab=
	"PC1 (54.85%))",ylab="PC3 (13%)",choices=c(1,3))
dev.off()

loadings(pca2)


#=======================================================================

# Kernel regularized least sqaures------------------------------
Chemical_Potential
# matriz com as repostas 

df.krls.resp <-data.frame(Yield,Conv,Selectivity)
df.krls.var <-data.frame(Basicity,Hardness,Softness)

mod1 <-krls(X=df.krls.var,y=df.krls.resp[,1])
summary(mod1)

mod2 <-krls(X=df.krls.var,y=df.krls.resp[,2])
summary(mod2)

mod3 <-krls(X=df.krls.var,y=df.krls.resp[,3])
summary(mod3)
plot(mod3)

#=======================================================================

fit1 <-aov(Selectivity~Anion)
summary(fit1)
coefficients(fit1)

fit2 <-aov(Hardness~Anion)
summary(fit2)
coefficients(fit2)
