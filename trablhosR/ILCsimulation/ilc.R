# análises estatísticas das simulações computacionais 

## Etapas ===================================================================

# 0. Packages and otheres alilke 
# 1. Matrix reading, loading and creation 
# 2. Exploratory data analysis  
# 3. Simulations 
# 4. Plots 

# 0. packages and directories ===============================================

# load packages-------------------------------------------------------

.libPaths("/home/igor/R/x86_64-pc-linux-gnu-library/3.0")

library(caret)
library(pls)
library(kernlab)

# Set directories-----------------------------------------------------

setwd("~")
setwd("r/ILCsimulation")

# 1.read archives ==========================================================

ilc <- read.delim("ilc")
# removing unbalanced objects 
ilc <- subset(ilc,tempo=="12")
# removing variable without variance 
ilc <-ilc[,-4]# tirando yield
ilc <-ilc[,-2]
ilc <-ilc[,-2]
ilc <-ilc[-6,]
attach(ilc)
View(ilc)
# Removing yield

# perfil das variáveis------------------------------------------------------
str(ilc)
names(ilc)
 [1] "IL"                 "Selc"               "Yield"             
 [4] "F_eleto"            "F_Nucleo"           "Hardness"          
 [7] "Softness"           "Eletronegativity"   "Acidity"           
[10] "Basicity"           "Chemical_Potential" "Homo"              
[13] "Lumo"               "DeltaN"             "Relative.Hardness" 
[16] "Cation"             "Anion"
View(ilc)

# mtrix for simulated annealing 
attach(ilc)

ilc1 <-data.frame(
	Selectivity,Fukui_E,Fukui_N,Hardness,Basicity,
	Chemical_Potential,Softness,Acidity,Electronegativity,Homo,Lumo)


# 2- Exploratory data analysis================================================


## PCA--------------------------------------------------------------------------
rownames(ilc)<-ilc[,1] 
pca1 <-prcomp(ilc[,2:15],scale=T) # General PCA with scaled variables ---------
summary(pca1) 
biplot(pca1)
scores(pca1)
loadings(pca1)


pca2 <-prcomp(ilc[,2:15])
biplot(pca2)

# PCA contain relevant information ultil the fourth Principal component-------
biplot(pca1)

# plot1 ------------------------------------------------------------------------

tiff("pca01.tiff", width = 10, height = 6, units = 'in', res = 200)
par(mfrow=c(1,2)) 
biplot(pca1,xlim=c(-0.6,0.5),xlab="PC1(48%)",ylab="PC2(27%)")
biplot(pca1,xlim=c(-0.6,0.5),xlab="PC1(48%)",ylab="PC3(10%)",choice=c(1,3))
dev.off()

tiff("pca1.2.tiff", width = 7, height = 7, units = 'in', res = 200)
biplot(pca1,xlim=c(-0.6,0.5),xlab="PC1(48%)",ylab="PC2(27%)")
dev.off()

tiff("pca1.3.tiff", width = 7, height = 7, units = 'in', res = 200)
biplot(pca1,xlim=c(-0.6,0.5),xlab="PC1(48%)",ylab="PC3(10%)",choice=c(1,3))
dev.off()

# 3. simulated annealing searches and models =====================================

# stting methods  ---------------------------------------------------------------
ctrl2 <- safsControl(functions = rfSA,
                    method = "cv",
                    number = 7)
# simulation programming --------------------------------------------------------
rf_search1 <- safs(x = ilc1[,-1],
                  y = ilc1[,1],
                  iters = 300,
                  safsControl = ctrl2)

# Predicted vs reference with the best model----------------------------------
Predicted <-predict(rf_search1,newdata=ilc1[,-1]) #or 
Predicted <-c(52.26050,37.98230,45.55218,47.98510,58.92381,48.40942,41.16688,
39.90685,38.95495,38.69112,62.29175,47.10594, 52.39287,40.2867,38.37506)

Reference <-ilc[,3]

# plotting predicted vs Reference  -------------------------------------------
tiff("pvsref.tiff", width = 6, height = 7, units = 'in', res = 200)
plot(Predicted~Reference,xlab="Reference",main="Simulated Annealing model for Selectivity")
dev.off()

# melhor safs------------------------------------------------------------------
Internal performance values: RMSE, Rsquared
Subset selection driven to minimize internal RMSE 

External performance values: RMSE, Rsquared
Best iteration chose by minimizing external RMSE 
External resampling method: Cross-Validated (7 fold) 

During resampling:
  * the top 5 selected variables (out of a possible 10):
    Hardness (100%), Softness (85.7%), Lumo (71.4%), Acidity (42.9%), Basicity (28.6%)
  * on average, 3.4 variables were selected (min = 1, max = 5)

In the final search using the entire training set:
   * 4 features selected at iteration 159 including:
     Fukui_N, Hardness, Softness, Lumo  
   * external performance at this iteration is

      RMSE   Rsquared 
     13.14       1.00 



# 4. Plots ==============================================================

attach(ilc)

# plots de exploração ----------------------------------------------------
plot(Homo~Selc)
plot(log(Selc)~log(Acidity))
plot(log(Selc)~log(F_eleto))
plot(log(1/Selc)~log(F_eleto))
plot(Selc~exp(F_Nucleo))

plot(log(1/Selc)~(Acidity)) # 2
plot(log(1/Selc)~log(Acidity))
plot((1/Selc)~(Acidity)) 
plot((1/Selc)~log(Acidity))



plot(log(Selc)~log(Hardness)) # ver agrupamentos; 

plot((Selc)~(Softness)) # ver agrupamentos; retirar outliers; usar ggplot

v1 <-log(1/Selc)
v2 <-log(F_eleto)
v3 <-log(Hardness)

ilc0 <-data.frame
(Selectivity=v1,Fukui_Electrophilicity=v2,Hardness=v3,Acidity,Softness)

pca01 <-prcomp(ilc0)
summary(pca01)

a <-exp(-Selc/8)



plot(exp(-Selectivity/8)~(Softness),data=ilc1[-6,]) # 2 e 6 
plot(log(Selc)~log(Hardness),data=ilc[-6,])  # 6
plot(log(1/Selc)~log(F_eleto)) # 12 
plot(log(1/Selc)~(Acidity)) #2 

plot(Selc~exp(-Basicity),data=ilc[-6,]) # show de bola; 
xyplot(Selc~exp(-Basicity),data=ilc[-6,],groups=Anion)
plot(Selc~Eletronegativity)
plot(log(Selc)~Basicity)

#plots com ggplot2 -------------------------------------------------------------
ilc <-ilc[-6,]
attach(ilc)
a <- exp(-Selectivity/8)
b <- exp(-Selectivity/16)
c <- exp(-Basicity)

#-------------------------------------------------------------------------------

p1 <-ggplot(ilc,aes(y=a,x=c))+
  geom_point(shape=1) +
  geom_smooth(method=lm)
tiff("corplot1.tiff", width = 6, height = 7, units = 'in', res = 200)
p1 + labs(x="exp(-Basicity)",cex=1.5)+
     labs(y="Selectivity",cex=1.5)
dev.off()

#------------------------------------------------------------------------------

p2 <-ggplot(ilc,aes(y=a,x=Softness)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)
tiff("corplot2.tiff", width = 6, height = 7, units = 'in', res = 200)
p2 + labs(y="exp(-Selectivity/C)",cex=1.5)+
     labs(x="Softness(eV)",cex=1.5)
dev.off()

#-------------------------------------------------------------------------------

p3 <-ggplot(ilc,aes(y=b,x=Fukui_E)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)
tiff("corplot3.tiff", width = 6, height = 7, units = 'in', res = 200)
p3 + labs(x="Fukui Eletrophilicity",cex=1.5)+
     labs(y="exp(-Selectivity/C)",cex=1.5)
dev.off()

#--------------------------------------------------------------------------------

# outros descritores-------------------------------------------------------------

localSoftnessN <- Softness*Fukui_N
localSoftnessE <- Softness*Fukui_E

plot(Selectivity~localSoftnessN)
plot(Selectivity~localSoftnessE)
plot(Selectivity~log(localSoftnessE))

# boxplots -----------------------------------------------------------------------

detach(ilc)
ilc <-ilc[-6,]
attach(ilc)

detach(ilc)
ilc <-ilc[-2,]
attach(ilc)
detach(ilc)

#---------------------------------------------------------------------------------
tiff("plot05.tiff", width = 8, height = 8, units = 'in', res = 200)
par(mfrow=c(2,2))
plot(Selectivity~Cation,ylab="Selectivity(eV)")
plot(Selectivity~Anion,ylab="Selectivity(eV)")
plot(Hardness~Cation,ylab="Hardness (eV)")
plot(Hardness~Anion,ylab="Hardness (eV)")
dev.off()

#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
tiff("plot089.tiff", width = 8, height = 8, units = 'in', res = 200)
par(mfrow=c(2,2))
plot(Fukui_E~Anion,ylab="Fukui Function Electrophilicity")
plot(Fukui_N~Anion,ylab="Fukui Function Electrophilicity")
plot(Fukui_E~Cation,ylab="Fukui Function Nucleophilicity")
plot(Fukui_N~Cation,ylab="Fukui Function Nucleophilicity")
dev.off()
#---------------------------------------------------------------------------------
tiff("plot07.tiff", width = 8, height = 8, units = 'in', res = 200)
par(mfrow=c(2,2))
plot(Acidity~Anion,ylab="Acidity (eV)")
plot(Basicity~Cation,ylab="Basicity (eV)")
plot(Acidity~Cation,ylab="Acidity (eV)")
plot(Basicity~Anion,ylab="Basicity (eV)")
dev.off()
#---------------------------------------------------------------------------------
tiff("plot06.tiff", width = 8, height = 10, units = 'in', res = 200) 
par(mfrow=c(2,2))
 plot(Hardness~Cation,ylab="Conversion (%)")
plot(Selectivity~Cation,ylab="Selectivity(eV)")
plot(Selectivity~Anion,ylab="Selectivity(eV)")
 plot(Hardness~Anion,ylab="Hardness (eV)")
dev.off()
#-------------------------------------
#criando relative hardness 
Relative.Hardness <-Hardness/6.8617
#-----------------------------------



