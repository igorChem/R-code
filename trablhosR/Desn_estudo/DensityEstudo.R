# Estudo da dendisdade de soluções de liquidos ionicos 


# Índice 

# 0. First ajusts 
# 1. Load data and create tables and variables
# 2. Plots and exploratory data 
# 3. Modelos de densidade por concentração 
# 4. Modelos de densidade por índice de refração (geral)
# 5. Modelo para densidade em função dos índices de refração 
# 	5.1 bmimBF4
#	5.2 sles 

#=======================================================================
# 0. first adjusts

# set dir --------------------------------------------------------------

setwd("~/")
setwd("~/Dropbox/r/Desn_estudo")

# load packages---------------------------------------------------------

.libPaths("/home/igor/R/x86_64-pc-linux-gnu-library/3.0")

library(pls)
library(caret)
library(KRLS)

#=======================================================================

# 1.Load data

#tables-----------------------------------------------------------------

tab1 <-read.delim("dens") # tabela primária de dados

tab2 <-subset(tab1,IL=="[BMIM][BF4]") #tabela para modelo bmimbf4
tab2 <-subset(tab2,!massfractionIL==1)

# cores para a função persp---------------------------------------------

# função para gerar ariável de cor para a função persp------------------


gridColors <-function(z) {
	jet.colors <- colorRampPalette(c("purple",
	"blue", "green","yellow","orange","red"))

	nbcol <- 1000
	color <- jet.colors(nbcol)
	nrz <- nrow(z)
	ncz <- ncol(z)
	zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
	return(facetcol <- cut(zfacet, nbcol)) 	
	}

#=======================================================================

# 2. Plots and exploratory data-----------------------------------------

attach(tab1) 

#Estudo da tabela 1-----------------------------------------------------

names(tab1)
head(tab1)
summary(tab1)
str(tab1)

# plots da densidade ---------------------------------------------------

plot(Density~Temperature.K.) ## interessante trabalhar nisso com lattice
#-----------------------------------------------------------------------
plot(Density~massfractionIL) # esse é o mais provável
plot(Density~molarfractionIL) # esse não da 
plot(Density~C.mol.L.) # mal distribuido 
#-----------------------------------------------------------------------

# boxplots--------------------------------------------------------------

boxplot(Density~Alkyl.Lenght)
boxplot(Density~MW)
boxplot(Density~Mwanion)
boxplot(Density~Mwcation)
boxplot(Density~IL)

#-----------------------------------------------------------------------

xyplot(Density.pure.~Temperature.K.*(Density.pure.-1)
				   |Anion,auto.key=T)

xyplot(Density~(massfractionIL*(Density.pure.-0.997))
				      |IL,auto.key=T)

# Dados de índice de refração-------------------------------------------

bwplot(Refractive.index)
bwplot(Density)

densityplot(Refractive.index)

'''
tiff("refvsden.tif",width=6,height=6,units="in",res=200)
xyplot(Density~Refractive.index,
       data=tab2,xlab="Índice de Refração",ylab="Densidade (g/cm³)",        
       groups=y,auto.key=list(space="right"),drop.unused.levels=T) 
'''

dev.off()

# faltando um ajuste----------------------------------------------------

plot(Refractive.index~molarfractionIL) # bem linear 
plot(Refractive.index~Densitynsity.pure.) # sem relação 
plot(Refractive.index~MW)
plot(Refractive.index~Mwcation)
plot(Refractive.index~Mwanion)
plot(Density~molarfractionIL)

#=======================================================================
# 3. Modelos 

# 3.1 modelo multivariado ----------------------------------------------

# 3.1.1 variáveis de datasets-------------------------------------------

tab4 <-subset(tab1,!Density=="NA") 
tab4 <-subset(tab1,!Density.pure.=="NA")
attach(tab4)
mf <-massfractionIL*(Density.pure.-1) # variável de concentração

# 3.1.2 modelo ---------------------------------------------------------

mod1 <- lm(Density~mf+Temperature.K.)
summary(mod1)
predplot(mod1) 

# 3.1.3 plots ----------------------------------------------------------

# predplot--------------------------------------------------------------
tiff("predplot.tiff", width = 6, height = 7, units = 'in', res = 125)
predplot(mod1,ylab="Densidade Prevista",xlab="Densidade experimental")
dev.off()

# residuals por anions--------------------------------------------------
tiff("erroanion.tiff", width = 8, height = 6, units = 'in', res = 125)
plot(residuals(mod1)~Anion,data=tab4)
dev.off()

# residuals por cations-------------------------------------------------
tiff("errocation.tiff", width = 6, height = 7, units = 'in', res = 125)
plot(residuals(mod1)~Cation)
dev.off()

#-----------------------------------------------------------------------

# 3d plot; response curve----------------------------------------------- 

# Definir as variavais para a função -----------------------------------

MFdp <-seq(0,0.3,length=30)
Temperatura <-seq(298,343,length=30)

#Montar a função que resultara em densidade

f1 <-function(MFdp,Temperatura) 
	{1.069609-0.000224*Temperatura+0.989895*MFdp}

# calcular a densidade para o produto matricial

Densidade <-outer(MFdp,Temperatura,f1)

gridColors(z=Densidade)


# curva de resposta plot

tiff("3dplot",width = 6, height = 7, units = 'in', res = 125)
      persp(MFdp,Temperatura,Densidade,
	phi = 20,theta = -40,shade=T ,
	ticktype = "detailed",
	col=color[facetcol],
	ylab="Temperatura (C°)",
	xlab="MFdp",
	zlab="Densidade(g/cm³)")
dev.off()


# 3.2 Machine learning -------------------------------------------------

# modelo de KRLS
# tabelas e variáveis---------------------------------------------------

w <- data.frame(MFdp=mf,
		Temperature.K.,MManion=Mwanion,MMcation=Mwcation,Density)
w <- na.omit(w) 

# creating partition for train and test sets----------------------------
set.seed(107)
inTrain <-createDataPartition(y=w[,5],
			     p=0.8,
			     list=FALSE)
TrainW <-w[inTrain,]
TestW  <-w[-inTrain,]

# Modelo ---------------------------------------------------------------

krls01 <-krls(X=TrainW[,-5],y=TrainW[,5])

summary(krls01)

#plots do modelo -------------------------------------------------------

predicted <-predict.krls(krls01,newdata=TestW[,-5])
test <-cbind(TestW[5],predicted$fit)

tiff("predkrls",width = 6, height = 7, units = 'in', res = 125)

plot(fitted(krls01)~TrainW[,5],col="blue",ylab="Densidade estimada",
            xlab="Densidade experimental",main="modelo KRLS")
            points(TestW[,5],predicted$fit,col="red")
            legend("bottomright",legend=c("Dados de Teste","Dados do modelo"),
            pch=c(1,1),lwd=c(2,2),col=c("red","blue"),cex=.8)
dev.off()

#-----------------------------------------------------------------------

tiff("krls1",width = 6, height = 7, units = 'in', res = 125)
par(mfrow=c(2,2))
plot(krls01)
dev.off()



#=======================================================================

# 4. Modelo por Índice de refração -------------------------------------

attach(tab2)

# modelos lineares utilizados para fazer modelo em função do índice de refração
fit1 <-lm(Density~Refractive.index)
summary(fit1)

tiff("pred01.tif",width=6,height=6,units="in",res=200)
predplot(fit2,ylab="Densidade Prevista",xlab="Densidade experimental")
dev.off()



# modelos lineares para bmimbf4-----------------------------------------

mod1.lm <-lm(Density~molarfractionIL)
summary(mod1.lm)

#-----------------------------------------------------------------------
Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)     0.9932922  0.0008725  1138.5   <2e-16 ***
molarfractionIL 2.0404843  0.0190919   106.9   <2e-16 ***
#-----------------------------------------------------------------------

mod2.lm <-lm(molarfractionIL~Refractive.index)
summary(mod2.lm)




# densidade em função do índice de refração para bmimbf4 ---------------

density.mod2.lm <-function (RI,temperatura) {

	t <-temperatura
	ri=RI-(t-20)*0.00045	 
 	ri2 =  2.0404843*(0.56399*ri-0.73670)+0.9932922
	density.bf4= ri2-(20-t)*0.00045	
        return(density.bf4)} 


# teste de amostra------------------------------------------------------
inp1 <-read.delim("inp1")
density.mod2.lm(inp1[,1],inp1[,2])

# 4.1 modelo krls para refractive index para vários IL------------------

attach(tab1)
wr <- data.frame(MFdp=mf,Density,Refractive.index)
wr <-na.omit(wr)

# creating partition for train and test sets----------------------------
set.seed(107)
inTrain <-createDataPartition(y=wr[,2],
			     p=0.8,
			     list=FALSE)
TrainWr <-wr[inTrain,]
TestWr  <-wr[-inTrain,]

# Modelo ---------------------------------------------------------------

krls02 <-krls(X=TrainWr[,-2],y=TrainWr[,2])

summary(krls02)

#-----------------------------------------------------------------------

summary(krls02)
* *********************** *
Model Summary:

R2: 0.9982881 

Average Marginal Effects:
                       Est Std. Error  t value     Pr(>|t|)
MFdp             0.5432612 0.03956363 13.73133 4.085815e-09
Refractive.index 0.1712200 0.01134283 15.09500 1.277051e-09

Quartiles of Marginal Effects:
                        25%       50%       75%
MFdp             0.19830068 0.4233611 0.9327373
Refractive.index 0.05495989 0.1768608 0.2938145


tiff("predkrls2",width = 6, height = 7, units = 'in', res = 125)

plot(fitted(krls02)~TrainWr[,2],col="blue",ylab="Densidade estimada",xlab="Densidade experimental",main="modelo KRLS")
     
dev.off()

# 4.3 modelo krls para refractive index para vários IL sem densidade do puro

dens <-(Density-1)
wr01 <- data.frame(dens,Refractive.index)
wr01 <-na.omit(wr01)

# creating partition for train and test sets---------------------------
set.seed(107)
inTrain <-createDataPartition(y=wr01[,1],
			     p=0.8,
			     list=FALSE)
TrainWr01 <-wr01[inTrain,]
TestWr01  <-wr01[-inTrain,]

# Modelo ---------------------------------------------------------------

krls03 <-krls(X=TrainWr01[,2],y=TrainWr01[,1])

summary(krls03)

tiff("predkrls2",width = 6, height = 7, units = 'in', res = 125)

plot(fitted(krls03)~TrainWr01[,1],col="blue",ylab="Densidade estimada",xlab="Densidade experimental",main="modelo KRLS")
     
dev.off()

