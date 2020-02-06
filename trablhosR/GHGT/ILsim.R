#GHGT 
# correlações para os dados computacionais 
# do trabalho do congresso da suiça 

#=======================================================================

# Summary

# 0. first ajusts 
# 1. load data 
# 2. Exploratory analisis
# 3. plots com ggplot2 
# 4. modelos multivariados 

#=======================================================================

# 0. first ajusts 


# set dir --------------------------------------------------------------

setwd("~/")
setwd("~/Dropbox/r/GHGT")

# Load Packages---------------------------------------------------------

.libPaths("/home/igor/R/x86_64-pc-linux-gnu-library/3.0")

library(caret)
library(pls)
library(gtools)
library(ggfortify)

#=======================================================================

# 1.Load data ----------------------------------------------------------

tab1 <-read.delim("hsabIL")
tab2 <-na.omit(tab1)

# data summary ---------------------------------------------------------

summary(tab1)
str(tab1)
names(tab1)

#-----------------------------------------------------------------------
 [1] "SiglaILRef"        "MM"                "nCarbonosLaterais"
 [4] "VolumeMolar"       "densidade"         "viscosidade"      
 [7] "TG"                "MP"                "DT"               
[10] "HC"                "CO2slubility"      "Ntf2"             
[13] "BF4"               "PF6"               "Cl"               
[16] "MIA"               "BMI"               "PY"               
[19] "MIB" 
#-----------------------------------------------------------------------

#=======================================================================

# 2. Exploratory Analysis-----------------------------------------------

#-----------------------------------------------------------------------
xyplot(Homo.eV.~Henry)
xyplot(Lumo.eV.~Henry)
xyplot(Fukui.eleto~Henry)
xyplot(Basicity~Henry|Cation)
xyplot(Acidity~Henry,type="smooth") # talvez valha a pena
xyplot(Acidity~1/Henry,type="smooth")
xyplot(Absolute.Hardness~Henry)
xyplot(Absolute.Softness~Henry)

plot(Fukui.Nucleo~Henry)
plot(chemical.electronic.potential~exp(-Henry)) # parece interessante
plot((log(1/Total.Eletrophilicity))~log(Henry))

#-----------------------------------------------------------------------
plot(exp(Homo.eV.)~Entropy)
plot(Lumo.eV.~Entropy)
plot((chemical.electronic.potential^3)~Entropy)
plot(Entropy~chemical.electronic.potential)
plot(Henry~Entropy)
plot(Fukui.eleto~Entropy)
plot(Fukui.Nucleo~Entropy)
plot(Absolute.Hardness~Entropy)
w <-1/Entropy
plot(Absolute.Softness~w)

plot(Acidity~Entropy)
plot(Basicity~Entropy)# could have some relationship
#-----------------------------------------------------------------------
plot(Absolute.Hardness~Enthalpy)
plot(Absolute.Softness~Enthalpy)
plot(chemical.electronic.potential~Enthalpy)
plot(Lumo.eV.~Enthalpy)
plot(Fukui.eleto~Enthalpy)
plot(Fukui.Nucleo~Enthalpy)
plot(Acidity~Enthalpy)
plot(Basicity~Enthalpy)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
attach(data)
plot(Henry~Absolute.Hardness)
plot(Henry~Absolute.Softness)
plot(Henry~Basicity)
plot(log(Henry)~log(Acidity))
plot(log(Henry)~log(Total.Eletrophilicity)) # estudar a retirada de outliers 
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
plot(Enthalpy~Absolute.Hardness)
plot(Enthalpy~Absolute.Softness)
plot(Enthalpy~Acidity) # legal
plot(Enthalpy~Basicity)
plot(Enthalpy~Total.Eletrophilicity)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
plot(Entropy~Absolute.Hardness)
plot(Entropy~Absolute.Softness)
plot(Entropy~Acidity) # legal
plot(Entropy~Basicity)
plot(Entropy~Total.Eletrophilicity)
#-----------------------------------------------------------------------

plot(Henry~Acidity) # interessante 
plot(Entropy~Acidity) # interessante 
plot(Enthalpy~Acidity) # interessante 
plot(Enthalpy~Entropy)


plot(Henry~Absolute.Hardness) # 
plot(Entropy~Absolute.Hardness) # 
plot(Enthalpy~Absolute.Hardness)


plot(Henry~Total.Eletrophilicity) # 
plot(Entropy~Total.Eletrophilicity)
plot(Enthalpy~Total.Eletrophilicity)


# PCA-------------------------------------------------------------------

pca0 <-prcomp(tab1[1:8],scale=T)
biplot(pca0)

tab1 <-na.omit(tab1)
tab2 <-tab1[-3,]
pca1 <-prcomp(tab2,scale=T)
summary(pca1)
biplot(pca1)
fit5 <-lm(Henry~Acidity)
summary(fit5)
fit6 <-lm(Entropy~Acidity)
summary(fit6)
d <-cbind(Entropy,anions)
plot(Entropy~Anion,data = tab1)
plot(Entropy~Cation,data = tab1)
plot(Henry~Anion)

xyplot(Enthalpy~Acidity|Anion,data=tab1,groups = Cation,auto.key = T)
xyplot(Enthalpy~Acidity|Cation,data=tab1)

pls1 <-mvr(Enthalpy~Acidity+Basicity+Total.Eletrophilicity+Absolute.Hardness,method="oscorespls",validation="LOO")

summary(pls1)
R2(pls1)
plot(Henry~Acidity)
plot(Enthalpy~Acidity)
plot(Entropy~Acidity)
plot(Entropy~Enthalpy)
plot(Henry~Entropy)
plot(Henry~Enthalpy)

ggplot(tab1, aes(x=Henry, y=Acidity)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)

ggplot(tab1, aes(x=Enthalpy, y=Acidity)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)


g1 <-lo


tiff("EntropyvsAcidity.tiff",res=220,units="in",width=6,height=6)
p <-ggplot(tab2, aes(x=Enthalpy, y=Acidity)) +
  geom_point(shape=1,cex=2.5,col="red") +
  geom_smooth(method=lm)
  
p

p + labs(x="Enthalpy(J/mol.K)",cex=1.5)+
     labs(y="Acidity(eV)",cex=1.5)
     

dev.off()

#-----------------------------------------------------------------------

tab3 <-tab2[-6,]

tiff("EntropyvsEletrophilicity.tiff",res=220,units="in",width=6,height=6)
p <-ggplot(tab3, aes(x=Henry, y=Total.Eletrophilicity)) +
  geom_point(shape=1,cex=2.5,col="red") +
  geom_smooth(method=lm)
  
p
  
p + labs(x="Henry constant",cex=1.5)+
    labs(y="Eletrophilicty(eV)",cex=1.5) 
dev.off()

#-----------------------------------------------------------------------

tiff("EntropyvsAcidity.tiff",res=220,units="in",width=6,height=6)
p <-ggplot(tab2, aes(x=Henry, y=Absolute.Softness)) +
  geom_point(shape=1,cex=2.5,col="red") +
  geom_smooth(method=lm)
  
p

p + labs(x="Enthalpy(J/mol.K)",cex=1.5)+
     labs(y="Acidity(eV)",cex=1.5)
     

dev.off()

#-----------------------------------------------------------------------

j <-ggplot(tab1, aes(x=Entropy, y=Absolute.Hardness)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)
j

g <-ggplot(tab1, aes(x=Entropy, y=Basicity)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)
g

t <-ggplot(tab1, aes(x=Henry, y=Total.Eletrophilicity)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)
t
summary(t)



















 
