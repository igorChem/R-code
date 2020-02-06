## modelos mistura para reunião do dia 22

# modelos de mistrua de IL, água e MDEA 

# 0.First adjusts 
# 1. Load data 
# 2. Modelo para viscosidade 
# 3. Modelo paa densiddade
# 4. Modelo para Cp 
# 4. Modelo de preço e absorção
# 5. modelo multicriterio
# 6. graphs

#=========================================================================

# 0.First adjusts 

#setting working directory--------------------------------------------------4

setwd("~/Dropbox/r/MixLIMDEA")

# setwd("~/igor/Dropbox/Dropbox/r/MixLIMDEA")

library(KRLS)
library(pls)
library(mixexp) 
library(lattice)

# functions ----------------------------------------------------------

sdtab <- function (tab,cv=F) {

	if (cv==F) {
	x <-apply(tab,2,sd)}
    else { x <-apply(tab,2,sd)/apply(tab,2,mean) }
    print(x)
    return(x)    
	}

gridColors <-function(z) {
	jet.colors <- colorRampPalette(c("purple",
	"blue", "green","yellow","orange","red","pink"))

	nbcol <- 1000
	color <- jet.colors(nbcol)
	nrz <- nrow(z)
	ncz <- ncol(z)
	zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
	return(facetcol <- cut(zfacet, nbcol)) 	
	}


write.lm.results <-function(model,vars,nvars...) {
		
		
		anovatable <-summary(model)[4]		
		x <-matrix(unlist(anovatable),nrow=)
		Intercept <- x[,1]
		df <-data.frame(x[,2:(nvars+2)],row.names=vars)
		df <-data.frame(Intercept,x)
		print(df) }
		


jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red"))

# 1. load data-----------------------------------------------------------

viscosity <-read.table("viscosity",header=T)

maintab <-read.table("sorption",header=T)

heat <-read.table("heatcapacity",header=T)


#=======================================================================

# 2. modelo para viscosidade

attach(viscosity)

# summary data for viscosity table
names(viscosity)
summary(viscosity)
sdtab(viscosity)
sdtab(viscosity,cv=T)

# lista de parametros para as legendas dos plots lattice
Key <-list(space="right",points=list(pch=1:2,col=c("blue","red")),
text=list(names(viscosity)[1:2]))


# plotar esse
tiff("plot01.tiff",res=220,units="in",width=6,height=6)
xyplot(xLI+xMDEA~viscosidade.MPa.s.|Temperatura,data = viscosity,
key=Key,pch=1:2,col=c("blue","red"),xlab="Viscosidade (MPa.S)",
ylab="xLI  e  xMDEA")
dev.off()


# plotar esse
tiff("plot02.tiff",res=220,units="in",width=6,height=6)
boxplot(exp(viscosidade.MPa.s.)~Temperatura,ylab="Viscosidade (MPa.S)",
xlab="Temperatura (K)")
dev.off()

# modelos lm


# modelo linear utilizado 
mod.lm4 <-lm(log(viscosidade.MPa.s.)~xLI+xMDEA+Temperatura) 
summary(mod.lm4)
coeffcients(mod.lm4)

tiff("plot03",res=220,units="in",width=6,height=6)
predplot(mod.lm4,main="",ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(mod.lm4$fitted.values~log(viscosidade.MPa.s.)),col="red")
dev.off()

tiff("plot0010",res=220,units="in",width=6,height=6)
plot(fitted.values(mod.krls1)~viscosidade.MPa.s.,main="",ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(fitted.values(mod.krls1)~viscosidade.MPa.s.),col="red")
dev.off()

#mod krls 

#treinar esse modelo
mod.krls1 <-krls(X=viscosity[,1:4],y=viscosity[,5])
summary(mod.krls1)
plot(mod.krls1) # muito bom 

	tiff("plot04.tiff",res=220,units="in",width=6,height=6)
	
	dev.off()
	
#-----------------------------------------------------------------------

detach(viscosity)

#=======================================================================

# 3. modelos para densidade 

# summary data para maintab
names(maintab)
summary(maintab)
sdtab(maintab)
sdtab(maintab,cv=T)

attach(maintab)

# plot da densidade em função da temperatura
tiff("plot05",res=220,units="in",width=6,height=6)
boxplot(Densidade~Temperatura,
	xlab="Temperatura (K)",ylab="Densidade (g/cm³)")
dev.off()
	
# lista de parametros para legenda 
Key1 <-list(space="right",points=list(pch=1:2,col=c("blue","red")),
text=list(c("303.15K","323.15K")))

tiff("plot06",res=220,units="in",width=6,height=6)
xyplot(xIL+xMDEA~Densidade,data=maintab,groups=Temperatura,
key=Key1,pch=1:2,col=c("blue","red"),xlab="Densidade (g/cm³)",
ylab="Fração Mássica",cex=1.1)
dev.off()

# modelo lm

# modelo linear 
mod.lm7 <-lm(Densidade~xLI+xMDEA+Temperatura)
summary(mod.lm7)

# predplot para modelo linear para densidade
tiff("plot07",res=220,units="in",width=6,height=6)
predplot(mod.lm7,main="",ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(fitted.values(mod.lm7)~Densidade),col="red")
dev.off()


# modelo de mistura 

dat20 <-maintab[,1:3]
modmix001 <-MixModel(dat20,"Densidade",mixcomps=names(dat20),1,procvars="Temperatura")
summary(modmix001)

# predplot para modelo de mistura para densidade
tiff("plot08",res=220,units="in",width=6,height=6)
predplot(modmix001,main="",ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(fitted.values(modmix001)~Densidade),col="red")
dev.off()

#esses dois plots 

tiff("plot09",res=220,units="in",width=9,height=8)
ModelPlot(modmix001,dimensions=list(x1="xIL",x2="xMDEA",x3="xH2O"),
main="Densidade (g/cm³)",
contour=T,cuts=10,fill=T,axislabs = c("xBMIMBF4","xMDEA","xH2O")
,cornerlabs =c("xBMIMBF4", "xMDEA", "xH2O")
,colorkey=T,color.palette=jet.colors)
dev.off()

tiff("plot10",res=220,units="in",width=9,height=8)
ModelPlot(modmix001,dimensions=list(x1="xLI",x2="xMDEA",x3="xH2O"),
slice = list(process.vars=c(Temperatura=323.15)),
main="Densidade (g/cm³)\n 323.15K",
contour=T,cuts=10,fill=T,axislabs = c("xBMIMBF4","xMDEA","xH2O")
,cornerlabs =c("xBMIMBF4", "xMDEA", "xWater")
,colorkey=T,color.palette=jet.colors)
dev.off()

detach(maintab)

#=======================================================================

# 4, modelos para cp 

attach(heat)


tiff("plo11.tiff",res=220,units="in",width=8,height=8)
par(mfrow=c(2,2))
plot(Cp~Temperatura,ylab="Cp (J/K.g)",xlab="Temperatura (K)")
boxplot(Cp~XMDEA,ylab="Cp (J/K.g)",xlab="xMDEA")
boxplot(Cp~XH2O,ylab="Cp (J/K.g)",xlab="xH2O")
boxplot(Cp~xLI,ylab="Cp (J/K.g)",xlab="xLI")
dev.off()


# modelo linear 
mod.lm8 <-lm(Cp~XH2O+xLI+XMDEA)
summary(mod.lm8)

# modelo krls

dat2 <-heat[,-2]

# muito bom o modelo, usar train function e 
mod.krls3 <-krls(X=dat2,y=Cp)
summary(mod.krls3)

tiff("plot12.tiff",res=220,units="in",width=8,height=8)
plot(fitted(mod.krls3)~Cp,ylab="Valores Calculados",
xlab="Valores Medidos")
abline(lm(fitted(mod.krls3)~Cp),col="red")
dev.off()

tiff("plot13.tiff",res=220,units="in",width=8,height=8)
plot(mod.krls3)
dev.off()


#-----------------------------------------------------------------------

detach(heat)

#=======================================================================

#modelos para absorção -------------------------------------------------

names(maintab)
summary(maintab)
sdtab(maintab)
sdtab(maintab,cv=T)

attach(maintab)

# plots exploratórios 
tiff("plot14",res=220,units="in",width=8,height=8)
par(mfrow=c(2,2))
boxplot(Fraction~xLI,ylab="Fração Molar CO2",xlab="xbmimbf4")
boxplot(Fraction~xMDEA,ylab="Fração Molar CO2",xlab="xMDEA")
boxplot(Fraction~xH2O,ylab="Fração Molar CO2",xlab="xH2O")
boxplot(Fraction~Temperatura,ylab="Fração Molar CO2",xlab="Temperatura")
dev.off()

# plots exploratorios 
tiff("plot15",res=220,units="in",width=8,height=8)
par(mfrow=c(2,2))
boxplot(molsCO2.kg~xIL,ylab="mols CO2/kg",xlab="xbmimbf4")
boxplot(molsCO2.kg~xMDEA,ylab="mols CO2/kg",xlab="xMDEA")
boxplot(molsCO2.kg~xH2O,ylab="mols CO2/kg",xlab="xH2O")
boxplot(molsCO2.kg~Temperatura,ylab="mols CO2/kg",xlab="Temperatura")
dev.off()

# modelo linear 
mod.lm11 <-lm(Fraction~xLI+xMDEA+xH2O+Temperatura)
summary(mod.lm11)

# predplot para modelo linear para absorção 

tiff("plot16",res=220,units="in",width=8,height=8)
predplot(mod.lm11,ylab="Valores Calculados",
xlab="Valores Medidos")
abline(lm(fitted(mod.lm11)~Fraction),col="red")
dev.off()

# power law 
mod.lm13 <-lm(log(Fraction)~xLI+xMDEA+Temperatura)
summary(mod.lm13)


tiff("predplot.tiffabsorfrac.tiff2",res=220,units="in",width=8,height=8)
predplot(mod.lm13,ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(exp(fitted(mod.lm12))~Fraction),col="red")
dev.off()

# matriz para modelos de mistura
dat5 <-maintab[,1:3]

modmix01 <-MixModel(dat5,"Fraction",mixcomps=names(dat5),2,procvars="Temperatura")
summary(modmix01)

modmix02 <-lm(Fraction~xIL+xMDEA+xIL:xMDEA+xH2O+Temperatura)
summary(modmix02)

tiff("plot17.tiff",res=220,units="in",width=8,height=8)
predplot(modmix02,ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(fitted(modmix01)~Fraction),col="red")
dev.off()

breakpoints1 <- c(0,0.04,0.08,0.12,0.16,0.20)
colors <- c("red","yellow","white","cyan","blue")

tiff("plot19",res=220,units="in",width=9,height=8)
ModelPlot(modmix02,dimensions=list(x1="xIL",x2="xMDEA",x3="xH2O"),
slice = list(process.vars=c(Temperatura=323.15)),
main="CO2 Fração molar",
contour=T,cuts=7,fill=T,axislabs = c("xBMIMBF4","xMDEA","xH2O")
,cornerlabs =c("xBMIMBF4", "xMDEA", "xH2O")
,colorkey=T,color.palette=jet.colors)
dev.off()

# melhor modelo de mistura

detach(maintab)

#=======================================================================

# 5. modelo multicriterio 


attach(maintab)

# prediction of Cp and viscosity 

datpred <- maintab[,1:4]
datpred1 <-data.frame(Temperatura=datpred[,4],datpred[,1:3])

Cp.p <-predict.krls(mod.krls3,newdata=datpred1)
Viscosidade <-predict.krls(mod.krls1,newdata=datpred)

n.maindata <-data.frame(maintab,Viscosidade$fit,Cp.p$fit)
detach(maintab)
attach(n.maindata)

# summary data 
summary(n.maindata)
sdtab(n.maindata)
sdtab(n.maindata,cv=T)
dim(n.maindata)


# análise de componentes principais 

pca.df <-n.maindata[,-5]
pca.df <-pca.df[,-7]

pca1 <-prcomp(pca.df,scale=T)
summary(pca1)
biplot(pca1)



# desirability functions 

d.fun <-function(var=a,inv=F) {
	
	x <-c()
	for (i in 1:length(var)) {
		if (var[i] <= min(var)) {x <-append(x,0)}
		else if (var[i] >= max(var)) {x <-append(x,1)}
		else {a <-(var[i]-min(var))/(max(var)-min(var));
		x <-append(x,a)}
		
	}
	if (inv==F) return(x)
	else return(exp(-x))
		 	
 }

d.viscosity <-d.fun(var=Viscosidade.fit,inv=T)
d.Cp <-d.fun(var=Cp.p.fit,inv=T)
d.Preço <-d.fun(Preço.R.kg.,inv=T)
d.Fração <-d.fun(Fraction)

D <-(d.viscosity*d.Cp*d.Preço*d.Fração)^(1/4)

mod.lmMC <-lm(D~xIL+xMDEA+xH2O+Temperatura)
summary(mod.lmMC)

n.maindata<-n.maindata[-10,]
D <-D[-10]
dataMC <-data.frame(n.maindata,D)
mixmmod.lmMC <-MixModel(dataMC,"D",mixcomps=names(dataMC[1:3]),1,
procvars="Temperatura")

summary(mixmmod.lmMC)

tiff("plot2112",res=220,units="in",width=6,height=6)
plot(fitted.values(mixmmod.lmMC)~D,main="D",ylab="Valores Calculados",xlab="Valores Medidos")
abline(lm(fitted.values(mixmmod.lmMC)~D),col="red")
dev.off()


tiff("plot",res=220,units="in",width=9,height=8)
ModelPlot(mixmmod.lmMC,dimensions=list(x1="xIL",x2="xMDEA",x3="xH2O"),
slice = list(process.vars=c(Temperatura=323.15)),
main="D",
contour=T,cuts=10,fill=T,axislabs = c("xBMIMBF4","xMDEA","xH2O")
,cornerlabs =c("xBMIMBF4", "xMDEA", "xH2O")
,colorkey=T,color.palette=jet.colors)
dev.off()

#
tiff("re.tiff",res=220,units="in",width=9,height=8)
par(mfrow=c(2,2))
plot(D~xIL,ylab="D",xlab="xLI")
plot(D~xH2O,ylab="D",xlab="xH2O")
plot(D~xMDEA,ylab="D",xlab="xMDEA")
plot(D~Temperatura,ylab="D",xlab="Temperatura")
dev.off()

#relação das respostas com suas funções de desejo

tiff("re2.tiff",res=220,units="in",width=9,height=8)
par(mfrow=c(2,2))
plot(Viscosidade.fit~d.viscosity,ylab="Viscosidade (Mpa.s)",xlab="d.viscosidade")
plot(Cp.p.fit~d.Cp,ylab="Cp (J/g.K)",xlab="d.Cp")
plot(Preço.R.kg.~d.Preço,ylab="Preço (U$/kg)",xlab="d.preço")
plot(Fraction~d.Fração,ylab="xCO2",xlab="d.xCO2")
dev.off()


tiff("re.tiff",res=220,units="in",width=9,height=8)
par(mfrow=c(2,2))
plot(D~d.viscosity,ylab="D",xlab="d.viscosidade")
plot(D~d.Cp,ylab="D",xlab="d.Cp")
plot(D~d.Preço,ylab="D",xlab="d.preço")
plot(D~d.Fração,ylab="D",xlab="d.xCO2")
dev.off()

