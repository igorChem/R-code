# Análises estatística sobre a produção de soja no brasil. 

#---------------------------------------------------------------------------
# Summary
#---------------------------------------------------------------------------

# 0. setdir and load packages 
# 1. read and load matrices 
# 2. explortory Analysis
# 3  PCA 
# 4. ANOVA 
# 5. CA
# 6. Segundo ANOVA 

#--------------------------------------------------------------------------------
# 0. setdir and load packages 
# -------------------------------------------------------------------------------

# set directory 

setwd("~/Dropbox/r/congACV")

# load packages 
library(lattice)
library(pls)
library(gplots)
library(ggplot2)
library(ggfortify)


# load data ====================================================================

mat <-read.delim("soja",row.names=1) 

# Anexar os dados ===============================================================

attach(mat)
names(mat)
summary(mat)

# subsetting to create only non-NA matrix======================================= 

mat1 <-subset(mat,!kg_CO2_Eq=="NA") 
mat2 <-subset(mat,!N=="NA")
mat01<-mat1[,-4]

#--------------------------------------------------------------------------------
# 2. analise exploratória
#--------------------------------------------------------------------------------

cov(mat[,3:11])
cor(mat[,3:11])

# plots de variaveis contra grupos regional-nacional ============================

plot(Produtividade~Region) # distribuição regional bem menor
plot(Defensivos~Region)# os valores nacionais são puxados por um valor extremo
plot(Ca~Region) # distribuição regional bem menor
plot(P~Region) # Distribuição regional bem menor, médias de diferenciam 
plot(kg_CO2_Eq~Region) # se distribuem de forma parecida, entretanto há falta de dados 
plot(semente~Region) # grande incerteza nos dados nacionais
plot(K~Region) # dados regionais com incerteza mínima, nacionais com incerteza normal.
plot(N~Region) # não há grandes diferenças

#----------------------------------------------------------------------------------
# Avaliar de a distribuição é normal
#---------------------------------------------------------------------------------

# testes de normalidade===========================================================

shapiro.test(Ca) #, p-value = 0.3164
shapiro.test(K)
shapiro.test(K^2) # p-value = 0.7302
shapiro.test(P) 
shapiro.test(P^2) # p-value = 0.3251
shapiro.test(semente) # p-value = 0.0207
shapiro.test(log1p(semente))
shapiro.test(Ano) # p-value = 0.6944
shapiro.test(log1p(Ano))
shapiro.test(kg_CO2_Eq) # p-value = 0.05962
shapiro.test(Produtividade) # p-value = 0.2362
shapiro.test(Defensivos) # p-value = 1.476e-06; tem outliers 
shapiro.test(Diesel) # p-value = 0.626
shapiro.test(N)

qqnorm(Diesel)
qqnorm(log(kg_CO2_Eq))
qqnorm(Ca)
qqnorm(exp(P^2))
qqnorm(Defensivos)
qqnorm

#boxplots=========================================================================

tiff("bxplot.tiff",width = 14, height = 8, units = 'in', res = 220)
par(mfrow=c(2,3))
plot(Produtividade~Region,xlab="",ylab="Produtividade (kg/ha)",main="Produtividade")
plot(K~Region,ylab="Potássio (kg/kg soja)",xlab="",main="Potássio")
plot(P~Region,ylab="Fósforo (kg/kg soja)",xlab="",main="Fósforo")
plot(Defensivos~Region,xlab="",ylab="Defensivos (kg/kg soja)",main="Defensivos")
plot(Diesel~Region,ylab="Diesel (kg/kg soja)",xlab="",main="Diesel")
plot(Ca~Region,ylab="Cálcio (kg/kg soja)",xlab="",main="Cálcio")
dev.off()

jpeg("bxplot.jpeg",width = 10, height = 6, units = 'in', res = 230)
par(mfrow=c(1,2))
plot(Defensivos~Region,xlab="")
plot(Diesel~Region,ylab="Diesel",xlab="")
dev.off()
# plots entre variáveis===========================================================

plot(Produtividade~Ca)
plot(Produtividade~K)
plot(produtividade~ano)

# lattice plots-----------------------------------------------------------------

xyplot(Produtividade~Ca|Region)
xyplot(Produtividade~kg_CO2_Eq|Region)
xyplot(Produtividade~K|Region)
xyplot(Produtividade~Ano|Region)
xyplot(Produtividade~N|Region)
xyplot(Produtividade~semente|Region)
xyplot(Produtividade~P|Region)
xyplot(Produtividade~Defensivos|Region)

# linear regression---------------------------------------------------------------

regCA <-subset(soja,Region=="Regional")
lmod1 <- lm(Produtividade~Ca,data=regCA)
summary(lmod1)

lmod2 <-lm(Produtividade~K,data=regCA)
summary(lmod2)

lmod3 <-lm(Produtividade~P,data = regCA)
summary(lmod3)

#--------------------------------------------------------------------------------
# 3. PCA 
#--------------------------------------------------------------------------------

pca1 <-prcomp(mat2[,3:11],scale=T)
summary(pca1)
loadings(pca1)
biplot(pca1) 

# biplots-------------------------------------------------------------------------
jpeg("Plot1.jpeg", width = 10, height = 7, units = 'in', res = 230)
biplot(pca1, ylab="PC2(23%)",xlab="PC1(30%)")
dev.off() 

# biplot entre o PC1 e o PC3-----------------------------------------------------
tiff("Plot2.tiff", width = 6, height = 6, units = 'in', res = 200)
biplot(pca1, choice=c(1,3),ylab="PC3(19%)",xlab="PC1(30%)",xlim=c(-0.7,0.5))
dev.off() 

jpeg("Plot1.jpeg", width = 10, height = 6, units = 'in', res = 230)
 par(mfrow=c(1,2))
 biplot(pca1, ylab="PC2(23%)",xlab="PC1(30%)")
 biplot(pca1, choice=c(1,3),ylab="PC3(19%)",xlab="PC1(30%)",xlim=c(-0.7,0.5))
dev.off()

tiff("Plot12.tiff", width = 6, height = 6, units = 'in', res = 200)
autoplot(pca1,xlab="PC1(30%)",ylab="PC2(23%)",
loadings=F,colour="Região",data=mat2,frame=T,loadings.label=F,size=3)
dev.off()

tiff("Plot12.tiff", width = 6, height = 6, units = 'in', res = 200)
autoplot(pca1,xlab="PC1(30%)",ylab="PC2(23%)",
loadings=T,data=mat2,frame=F,loadings.label=T,size=0)
dev.off()

#--------------------------plotting pca pc1 vs pc3----------------------
myPCAtrunc <- pca1
myPCAtrunc[[1]] <- myPCAtrunc[[1]][c(1,3,2,4)]
myPCAtrunc[[2]] <- myPCAtrunc[[2]][,c(1,3,2,4)]
colnames(myPCAtrunc[[2]]) <- c("PC1","PC2","PC3","PC4") # fake names
myPCAtrunc[[5]] <- myPCAtrunc[[5]][,c(1,3,2,4)]
colnames(myPCAtrunc[[5]]) <- c("PC1","PC2","PC3","PC4") # fake names
tiff("pca13.PC1vsPC3.tiff",res=200,units="in",width=6,height=6)
autoplot(myPCAtrunc, xlab = "PC1(32%)", ylab="PC3(19%)",
loadings=F,data=mat2,loadings.label=F,size=3,colour="Região",frame=T)
dev.off()
#-----------------------------------------------------------------------

#---------------------------------------------------------------------------------
# 4 Modelos ANOVA
#---------------------------------------------------------------------------------


# modelos para descartar a variável do ano----------------------------------------

fit0 <-lm(Produtividade~Ano)
summary(fit0)
anova(fit0)

# modelos de anova entre grupos regionais e nacionais---------------------------- 

#produtividade--------------------------------------------------------------------

fit1 <-aov(Produtividade~Region)
summary(fit1)

tiff("box1.tiff", width = 6, height = 5, units = 'in', res = 230)
boxplot(Produtividade~Region,xlab="Região",ylab="Produtividade (kg/ha)")
dev.off()

# TukeyHSD test-------------------------------------------------------------------

TukeyHSD(fit1)
plot(TukeyHSD(fit1)) 

#---------------------------------------------------------------------------------

# potássio------------------------------------------------------------------------
fit2<-aov(K~Region)
summary(fit2)

# TukeyHSD test-------------------------------------------------------------------

TukeyHSD(fit2)
plot(TukeyHSD(fit2)) 

#---------------------------------------------------------------------------------

# Nitrogênio----------------------------------------------------------------------
fit3 <-aov(N~Region)
summary(fit3)

# TukeyHSD test-------------------------------------------------------------------
TukeyHSD(fit3)
plot(TukeyHSD(fit3)) 

# Diesel-------------------------------------------------------------------------
fit4 <-aov(Diesel~Region)
summary(fit4)

# TukeyHSD test-------------------------------------------------------------------
TukeyHSD(fit4)
plot(TukeyHSD(fit4))

#Fósforo--------------------------------------------------------------------------
fit5 <-aov(P~Region)
summary(fit5)
# TukeyHSD test-------------------------------------------------------------------
TukeyHSD(fit5)
plot(TukeyHSD(fit5))

# Cálcio--------------------------------------------------------------------------
fit6 <-aov(Ca~Region)
summary(fit6)
# TukeyHSD test-------------------------------------------------------------------
TukeyHSD(fit6)
plot(TukeyHSD(fit6))
# Defensivos 
fit7 <-aov(Defensivos~Region)
summary(fit7)
# TukeyHSD test-------------------------------------------------------------------
TukeyHSD(fit7)
plot(TukeyHSD(fit7))
#CO2-eq---------------------------------------------------------------------------
fit8 <-aov(kg_CO2_Eq~Region)
summary(fit8)
# TukeyHSD test-------------------------------------------------------------------
TukeyHSD(fit8)
plot(TukeyHSD(fit8))


#---------------------------------------------------------------------------------
# 5. Verificação da possibilidade de outros agrupamentos 
#---------------------------------------------------------------------------------

# anáise de cluster===============================================================
ber <-rownames(mat2)
mat02 <-scale(mat2[,-1])
hc <-hclust(dist(mat02),method="single")

# plot do dendogram---------------------------------------------------------------
tiff("dendogram.tiff", width = 6, height = 6, units = 'in', res = 200)
plot(hc,hang = -1,labels = ber,main = "Dendograma",xlab="ICV",ylab="Distância",
sub="")
dev.off()


#---------------------------------------------------------------------------------
# 7.Análises dos novos grupos
#---------------------------------------------------------------------------------


# plots bivariados para os novos grupos-------------------------------------------
 
plot(Produtividade~grupos)
plot(Defensivos~grupos)
plot(Ca~grupos)
plot(P~grupos)
plot(kg_CO2_Eq~grupos)
plot(semente~grupos) 
plot(K~grupos)
plot(N~grupos)

# Modelos para dados primários e secundários-------------------------------------

plot(Ca~dataty)
plot(K~dataty)
plot(P~dataty)
plot(N~dataty)
plot(semente~dataty)
plot(Diesel~dataty)

#---------------------------------------------------------------------------------

#ANOVA para grupos----------------------------------------------------------------

fit1b <-aov(Produtividade~grupos)
summary(fit1)
 

fit2b <-aov(Defensivos~grupos)
summary(fit2)

fit3b <-aov(N~grupos)
summary(fit3)


fit4b <-aov(Diesel~grupos)
summary(fit4)


fit5b <-aov(P~grupos)
summary(fit5)

fit6b <-aov(Ca~grupos)
summary(fit6)


fit7b <-aov(Defensivos~grupos)
summary(fit7)

fit8b <-aov(kg_CO2_Eq~grupos)
summary(fit8)

# 7.3 Análise de diferenças entre grupos com TukeyHSD ---------------------------

TukeyHSD(fit1b) 

TukeyHSD(fit2b) 
TukeyHSD(fit3b)
TukeyHSD(fit4b) 
TukeyHSD(fit5b) 
TukeyHSD(fit6b) 
TukeyHSD(fit7b) 
TukeyHSD(fit8b)

plot(TukeyHSD(fit1b)) 
plot(TukeyHSD(fit2b)) 
plot(TukeyHSD(fit3b)) 
plot(TukeyHSD(fit4b)) 
plot(TukeyHSD(fit5b)) 
plot(TukeyHSD(fit6b)) 
plot(TukeyHSD(fit7b)) 
plot(TukeyHSD(fit8b))

# 8. -------------------------------------------------------------------

autoplot(pca1,xlab="PC1(32%)",ylab="PC2(26%)",
loadings=F,colour="Categoria.carpelo",data=tab1,frame=T)




