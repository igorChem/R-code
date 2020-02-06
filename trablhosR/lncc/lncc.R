# LNCC work 

# 0. First Things 
# 1. Loadings data 
# 2. Plots e LMs
# 3. Gráficos

#=======================================================================

#  0. First Things 

setwd("~/")
setwd("~/Dropbox/r/lncc")

.libPaths("~/R/x86_64-pc-linux-gnu-library/3.0")

library(caret) 
library(pls)
#=======================================================================

# 1. Load the data and packages

tab1 <-read.table("data",header=T) 

str(tab1) 

#=======================================================================

# 2. Plots e lms

splom(tab1[2:9])
attach(tab1)
plot(Conversão~log1p(DeltaN))
plot(Conversão~Dureza)

ac <-1/Acidity
ac1<-1/Acidity1

plot(Conversão~ac)
plot(Conversão~ac1)

plot(Conversão~1/Acidity1)
plot(Conversão~Basicity1)
plot(Conversão~Acidity)
plot(Conversão~Basicity)
plot(Conversão~Net.Basicity)
plot(Conversão~Chemical.Potential)

# PCA-------------------------------------------------------------------

pca1 <-prcomp(tab1[,-1],scale=T)

pca2 <- prcomp(tab1[-7,-1],scale=T)

# modelos lineares------------------------------------------------------

attach(tab1)
mod1 <- lm(Conversão~Acidity) 
summary(mod1)

p-value: 0.01646
R-squared:   0.6586
Error: 8.283 
#----------------------------------------------------------------------
mod2 <-lm(log(Conversão)~Acidity)
summary(mod2)

p-value: 0.002379
R-squared:  0.8384 
Error : 1.275111

#-----------------------------------------------------------------------

mod3 <-lm(exp(-Conversão/4)~Acidity)
summary(mod3)

p-value: 7.232e-06
R-squared:  0.9838 
Error : 0.007371

#=======================================================================

# 3. Graphs


j <-ggplot(tab1, aes(x=Acidity, y=exp(-Conversão/4)))+
  geom_point(shape=1) +
  geom_smooth(method=lm)+  
  labs(y="exp(-Conversão/4)",x="Eletronegatividade/Dureza")
  
  
tiff("plot1",res=220,width=6,height=6,units="in")
j
dev.off()

















