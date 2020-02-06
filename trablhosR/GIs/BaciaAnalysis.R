## Análise geoestátistica com análise multi-variada integrada 
# da Bacia Taquari-Antas

# ---------------------------------------------------------------# 
# Poluentes estimados por IPPS, concentração e dispersão ao longo# 
# dos trechos cálculadas usando as variáveis hidrológicas e os   #
# métodos de cálculo do Software IPH-Siqua 			 #
# As análises são de estátisticas descritiva, estudando a varição# 
# e distirbuição das variáveis de emissões e concentrações       #
# Análise exploratŕia 						 #
# Testes para analisar a dependência temporal dos dados          #
# Avaliação geoestatística, com modelos de distribuição espacial #
# Análise explorátoria de dados espaciais                        #
# Classificação de agrupamentos em relação a CONAMA              #  


# 0. Sumário-----------------------------------------------------#

# 0. Apresentação e Sumário--------------------------------------
# 1. Loading and verifying data----------------------------------
# 2. Functions---------------------------------------------------
# 3. Estátisticas descritivas------------------------------------
# 4. Análise de Componentes Principais e outros testes ----------
# 5. Avaliação dos fatores temporais nas concentraçãões ---------
# 6. Análise de distribuição espacial----------------------------
# 7. MVA para os mapas gerados-----------------------------------
# 8. classificação para as classes do Conama---------------------
# 9. Gráficos----------------------------------------------------

#=================================================================================

# 1. Loading and verifying data

# a.setting the working directory-------------------------------------------------

setwd("~/Dropbox/r/GIs")

# farminf 
setwd("~/igor/Dropbox/Dropbox/r/GIs")

# b. loading packages-------------------------------------------------------------

# .libPaths("/home/igor/R/x86_64-pc-linux-gnu-library/3.0")

library(sp)
library(raster)
library(maptools)
library(shapefiles)
library(fields)
library(caret)
library(rgdal)
library(pls)
library(fields)

# c.  Load the data --------------------------------------------------------------

# Emissões de metais na água -----------------------------------------------------

matab <-read.delim("matab")

attach(matab)
matab <-subset(matab,!ma05>(7*median(ma05))) # correção da variável
matab <-subset(matab,!ma05==0)
detach(matab) 

# Emissões de Tóxicos na água ----------------------------------------------------

tatab <-read.delim("tatab")

attach(tatab)
tatab <-subset(tatab,!ta05>mean(ta05))# correção da variável 
tatab <-subset(tatab,!ta05==0)
detach(tatab)

# Demanda bioquímica de oxigênio--------------------------------------------------

dbotab <-read.delim("dbotab")

attach(dbotab)
dbotab <-subset(dbotab,!dbo05>median(dbo05))
dbotab<-subset(dbotab,!dbo09>median(dbo09)) # correção da variável
dbotab <-subset(dbotab,!dbo05==0)
detach(dbotab)


# Sólidos totais na água----------------------------------------------------------

ststab <-read.delim("ststab") 

attach(ststab)
ststab <-subset(ststab,!sts05>median(sts05)) #correação da varíavel 
ststab <-subset(ststab,!sts07>median(sts07))
ststab <-subset(ststab,!sts05==0)
detach(ststab)

#---------------------------------------------------------------------------------

## Loading and rasterzing shape---------------------------------------------------

# dados para raster --------------------------------------------------------------

t1 <-readShapeSpatial("bacia_delim.shp",
                       IDvar="OBJECTID", 
			proj4string=CRS("+proj=longlat+ellps=clrk66"))
t01 <-extent(t1)
# create raster with the shape dimensions 
t001 <-raster(t01,nrow=500,ncol=500)
# transform the shapefile in raster from the latter created raster
t2 <-rasterize(t1,t001)

#=================================================================================

# 2. Functions-------------------------------------------------------------------

# função para fazer e plotar mapas 

#---------------------------------------------------------------------------------
EmissionMap <-function(data,y,dim,title,n=1,b=0,e=0.5) {	
	
	breakpoints0 <- c(0,0.1,0.2,0.4,0.6,0.8,1,1.2,10)
	breakpoints2 <- c(0,0.1,0.15,0.2,0.25,0.3,0.35,0.4,10)
	breakpoints1 <- c(0,0.1,0.5,1,1.5,2,2.5,3,10)
	breakpoints3 <- c(0,0.1,1,2,3,4,5,6,10)
		
	if (b==0) {breakpoints <- breakpoints0}
	if (b==1) {breakpoints <- breakpoints1}
	if (b==2) {breakpoints <- breakpoints2}
	if (b==3) {breakpoints <- breakpoints3}
    
	colors1 <- c("green4","green","yellowgreen","palegreen","yellow","orange","red","gray","gray")  
	colors2 <- c("green4","yellowgreen","yellow","red","red4")  
	
	t3 <-raster(t2)
	t3[] <-10
	t3 <-mask(t3,t2)
	
	mod <-Tps(data[,1:2],data[,y]^(1/n),m=dim)   
    	
	p <-raster(t2)                        
    p1 <-interpolate(p,mod)                
    p1 <-mask(p1,t2)    
    p1[p1<0] <-NA
    p2 <-interpolate(p,mod,fun=predictSE)
    p2 <-mask(p2,p1)
    p3 <-p2/p1
    p3[p3>e] <-NA
    p1 <-mask(p1,p3)
    p1 <-merge(p1,t3)
   
    plot(p1,breaks=breakpoints,col=colors,main=title)
       
   } # end 
#-----------------------------------------------------------------------------------


#--------------------------------------------------------------------------------

# função para a classificação conama em função das concentrações de Tóxicos-----

conamaClass <-function(Data,y,title,e=0.2,dim=2,PP="TA") {
    
    if (PP=="TA")  {limits <-c(2.2,2.2,3.3,3.3)}
    if (PP=="MA")  {limits <-c(0.092,0.092,0.520,0.520)}
    if (PP=="DBO") {limits <-c(1,10,50,100)}
    
    L <-limits 
    
    breakpoints <- c(0,1,2,3,4,10)
	colors <- c("green4","yellowgreen","yellow","red","gray","gray")  
   
     x <-c()
     G <-Data[,y]
	
	for (i in 1:length(G)) { 
		if (G[i] > L[4])  {z <-4}
        else if (G[i] > L[3]) {z <-3} 
        else if (G[i] > L[2]) {z <-2}
        else if (G[i] > L[1]) {z <-1}
        else {z <-0}         
        x <-append(x,z)}
	Data <- data.frame(Data,x)
	
    t3 <-raster(t2)
 	t3[] <-10
	t3 <-mask(t3,t2)
	
	mod1 <-Tps(Data[,1:2],Data[,y],m=dim) 
	
    mod2 <-Tps(Data[,1:2],Data$x,m=dim) 
    summary(mod2)
		
	p <-raster(t2)                        
	p1 <-interpolate(p,mod1)                
	p1 <-mask(p1,t2)    
	p1[p1<0] <-NA
	p2 <-interpolate(p,mod1,fun=predictSE)
	p2 <-mask(p2,p1)
	p3 <-p2/p1
	p3[p3>e] <-NA
	p1 <-mask(p1,p3)
	
	m1 <-interpolate(p,mod2)
	m1 <-mask(m1,p1)
	m1[m1>4] <-4
	m1 <-merge(m1,t3)
	
	plot(m1)
		
	plot(m1,breaks=breakpoints,col=colors,main=title)
    
    }

conamaClass(Data=tatab,y=4,title="W",PP="TA")

#---------------------------------------------------------------------------------

conamaClassDBO <-function(data,y,titulo,...) {
        
           
	x <-c()
        y <-data[,y]
	
	for (i in 1:length(y)) { 
		if (y[i] >10)  {z <-3}
                else if (y[i] >6) {z <-2}               
                else {z <-1}         
                x <-append(x,z)}
        df <-data.frame(data,x)
        EmissionMap(data=df,dim=3,y=length(data)+1,title=titulo)  
           }

#=================================================================================

# 3. Estátisticas descritivas

### matab ------------------------------------------------------------------------

attach(matab) 
summary(matab)
dim(matab) #dimensões da matriz (linhasXcolunas) 
[1] 62 19 # informação da matriz

#Plots de distribuição------------------------------------------------------------

tiff("matbwplot.tiff",units="in",width=12,height=6,res=200)
boxplot(matab[,4:15],ylab="MA mg/L")	
dev.off()

tiff("matbwplotlog.tiff",units="in",width=12,height=6,res=200)
boxplot(log1p(matab[,4:15]),ylab="log MA mg/L ")
dev.off()

tiff("matbwplotcubic.tiff",units="in",width=12,height=6,res=200)
boxplot(matab[,4:15]^(1/3),ylab="MA raiz cúbica mg/L")
dev.off()

# histograms 

hist(matab[,3],breaks=100,col="green")

tiff("histMA.tiff",units="in",width=6,height=7,res=200)
hist(matab[,3]^(1/3),breaks=15,col="green",main= "MA t/ano",xlab="") # melhor
dev.off()

hist(log1p(matab[,3]),breaks=30,col="green")

# histogramas da concentração em fução dos meses---------------------------------

tiff("histMAconc1.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(matab[,4]^(1/3),breaks=40,col="green",main="MA JAN",xlab="")
hist(matab[,5]^(1/3),breaks=40,col="green",main="MA FEV",xlab="")
hist(matab[,6]^(1/3),breaks=40,col="green",main="MA MAR",xlab="")
hist(matab[,7]^(1/3),breaks=40,col="green",main="MA ABR",xlab="")
hist(matab[,8]^(1/3),breaks=40,col="green",main="MA MAIO",xlab="")
hist(matab[,9]^(1/3),breaks=40,col="green",main="MA JUN",xlab="")
dev.off()

tiff("histMAconc2.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(matab[,10]^(1/3),breaks=40,col="green",main="MA JUL",xlab="")
hist(matab[,11]^(1/3),breaks=40,col="green",main="MA AGO",xlab="")
hist(matab[,12]^(1/3),breaks=40,col="green",main="MA SET",xlab="")
hist(matab[,13]^(1/3),breaks=40,col="green",main="MA OUT",xlab="")
hist(matab[,14]^(1/3),breaks=40,col="green",main="MA NOV",xlab="")
hist(matab[,15]^(1/3),breaks=40,col="green",main="MA DEZ",xlab="")
dev.off()

# testes de normalidade----------------------------------------------------------

# concentração de janeiro, representando a distribuição das outras---------------

qqnorm(ma01)
qqnorm(log1p(ma01))
qqnorm(ma01^(1/3))
shapiro.test(ma01)
shapiro.test(log1p(ma01))
shapiro.test(ma01^(1/3))

# Análise de componentes principais ---------------------------------------------

# i) para os dados númericos, sem transformação de distribuição------------------ 

attach(matab)
pca1DF <-data.frame(matab[,1:15],Número.de.Indústrias.MA,Metros)
pca1 <-prcomp(pca1DF,scale=T,center=T)

summary(pca1)

tiff("pca1.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca1,xlab="PC1(60%)",ylab="PC2(10%)")
biplot(pca1,choices=c(1,3),xlab="PC1(60%)",ylab="PC3(6%)")
dev.off()

scores.pca1 <- data.frame(Setor.Industrial.MA,pca1$x[,1:3])

pca1.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca1, 
	colour=factor(Setor.Industrial.MA),
	xlab="PC1 (59%)", ylab="PC2(10%)") + 
  	theme(legend.position="right") 

tiff("pca1.group",units="in",width=8,height=6,res=200)
pca1.pc1.2
dev.off()
	
pca1.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca1, 
	colour=factor(Setor.Industrial.MA),
	xlab="PC1 (59%)", ylab="PC3(6%)") + 
  	theme(legend.position="right") 

tiff("pca1.group2",units="in",width=8,height=6,res=200)
pca1.pc1.3
dev.off()

# ii) Par os dados númericos, com tranformação cpubica----------------------------

pca2DF <-data.frame(matab[,1:2],
                   (matab[,3:15]^(1/3)),Número.de.Indústrias.MA,Metros)

pca2 <-prcomp(pca2DF,scale=T,center=T)
summary(pca2)

tiff("pca2.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca2,xlab="PC1(62%)",ylab="PC2(10%)")
biplot(pca1,choices=c(1,3),xlab="PC1(60%)",ylab="PC3(6%)")
dev.off()

scores.pca2 <- data.frame(Setor.Industrial.MA,pca2$x[,1:3])

pca2.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca2, 
	colour=factor(Setor.Industrial.MA),
	xlab="PC1(59%)", ylab="PC2(10%)") + 
  	theme(legend.position="right") 

tiff("pca2.biplotgroup",units="in",width=8,height=6,res=200)
	pca2.pc1.2
dev.off()

pca2.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca1, 
	colour=factor(Setor.Industrial.MA),
        xlab="PC1(62%)", ylab="PC2(10%)") + 
  	theme(legend.position="right")
 
tiff("pca2.biplotgroup2",units="in",width=8,height=6,res=200)
	pca2.pc1.3 
dev.off()
detach(matab)      

#---------------------------------------------------------------------------------

### tatab ------------------------------------------------------------------------

attach(tatab) 
dim(tatab) 
[1] 106  18

#Plots de distribuição------------------------------------------------------------

tiff("tatbwplot.tiff",units="in",width=11,height=6,res=200)
boxplot(tatab[,4:15],ylab="TA mg/L")	
dev.off()

tiff("tatbwplotlog.tiff",units="in",width=11,height=6,res=200)
boxplot(log(tatab[,4:15]),ylab="log TA mg/L")
dev.off()

tiff("tatbwplotcubic.tiff",units="in",width=11,height=6,res=200)
boxplot(tatab[,4:15]^(1/3),ylab="TA raíz cúbica mg/L")	
dev.off()

#histogramas

hist(tatab[,3],breaks=100,col="green")

tiff("tathist.tiff",units="in",width=6,height=6,res=200)
hist(tatab[,3]^(1/3),breaks=40,col="green",xlab="",main="TA t/ano")
dev.off()

hist(log1p(tatab[,3]),breaks=50,col="green")

tiff("histTAconc1.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(tatab[,4]^(1/3),breaks=40,col="green",main="TA JAN",xlab="")
hist(tatab[,5]^(1/3),breaks=40,col="green",main="TA FEV",xlab="")
hist(tatab[,6]^(1/3),breaks=40,col="green",main="TA MAR",xlab="")
hist(tatab[,7]^(1/3),breaks=40,col="green",main="TA ABR",xlab="")
hist(tatab[,8]^(1/3),breaks=40,col="green",main="TA MAIO",xlab="")
hist(tatab[,9]^(1/3),breaks=40,col="green",main="TA JUN",xlab="")
dev.off()

tiff("histTAconc2.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(tatab[,10]^(1/3),breaks=40,col="green",main="TA JUL",xlab="")
hist(tatab[,11]^(1/3),breaks=40,col="green",main="TA AGO",xlab="")
hist(tatab[,12]^(1/3),breaks=40,col="green",main="TA SET",xlab="")
hist(tatab[,13]^(1/3),breaks=40,col="green",main="TA OUT",xlab="")
hist(tatab[,14]^(1/3),breaks=40,col="green",main="TA NOV",xlab="")
hist(tatab[,15]^(1/3),breaks=40,col="green",main="TA DEZ",xlab="")
dev.off()
# testes de normalidade----------------------------------------------------------

# concentração de janeiro, representando a distribuição das outras---------------
qqnorm(ta01)
qqnorm(log1p(ta01))
qqnorm(ta01^(1/3))
shapiro.test(ta01)
shapirotest(log1p(ta01))
shapiro.test(ta01^(1/3))

# Análise de componentes principais ---------------------------------------------

# i) para os dados númericos, sem transformação de distribuição------------------ 

attach(tatab)
pca3DF <-data.frame(tatab[,1:15],Número.de.Indústrias,Metros)
pca3 <-prcomp(pca3DF,scale=T,center=T)

tiff("pca3.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
summary(pca3)
biplot(pca3,xlab="PC1(60%)",ylab="PC2(10%)")
biplot(pca3,choices=c(1,3),xlab="PC1(60%)",ylab="PC3(6%)")
dev.off()

scores.pca3 <- data.frame(Setor_industrial,pca3$x[,1:3])

pca3.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca3, 
	colour=factor(Setor_industrial),
	xlab="PC1 (59%)", ylab="PC2(10%)") + 
  	theme(legend.position="right")
 
tiff("pca3.pc1.2",units="in",width=8,height=6,res=200)
pca3.pc1.2
dev.off()
	
pca3.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca3, 
	colour=factor(Setor_industrial),
        xlab="PC1 (59%)", ylab="PC3(6%)") + 
  	theme(legend.position="right") 

tiff("pca3.pc1.3",units="in",width=8,height=6,res=200)
pca3.pc1.3 
dev.off()

# ii) Par os dados númericos, com tranformação log--------------------------------

pca4DF <-data.frame(tatab[,1:2],(tatab[,3:15])^(1/3),Número.de.Indústrias,Metros)

pca4 <-prcomp(pca4DF,scale=T)
summary(pca4)

tiff("pca4.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca4,xlab="PC1(62%)",ylab="PC2(10%)")
biplot(pca4,choices=c(1,3),xlab="PC1(62%)",ylab="PC3(7%)")
dev.off()

scores.pca4 <- data.frame(Setor_industrial,pca4$x[,1:3])

pca4.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca4, 
	colour=factor(Setor_industrial),
	xlab="PC1(62%)", ylab="PC2(10%)") + 
  	theme(legend.position="right")
 
tiff("pca4.pc1.2",units="in",width=8,height=6,res=200)
pca4.pc1.2
dev.off()

pca4.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca4, 
	colour=factor(Setor_industrial),
         xlab="PC1(62%)", ylab="PC3(10%)") + 
  	theme(legend.position="right") 

tiff("pca4.pc1.3",units="in",width=8,height=6,res=200)
pca4.pc1.3
dev.off()

detach(tatab)
#---------------------------------------------------------------------------------

#dbotab------------------------------------------------------------------------

attach(dbotab) 
summary(dbotab)
dim(dbotab) #dimensões da matriz (linhasXcolunas) 
[1] 62 19

#Plots de distribuição------------------------------------------------------------

tiff("dbotbwplot.tiff",units="in",width=12,height=6,res=200)
boxplot(dbotab[,4:15],ylab="DBO mg/L")	
dev.off()

tiff("dbotbwplotlog.tiff",units="in",width=12,height=6,res=200)
boxplot(log(dbotab[,4:15]),ylab="log DBO mg/L")
dev.off()

tiff("dbotbwplotcubic.tiff",units="in",width=12,height=6,res=200)
boxplot(dbotab[,4:15]^(1/3),ylab="DBO raíz cúbica mg/L")	
dev.off()

# testes de normalidade----------------------------------------------------------

# concentração de janeiro, representando a distribuição das outras---------------

qqnorm(dboa01)
qqnorm(log1p(dbo01))
qqnorm(dbo01^(1/3))
shapiro.test(dbo01)
shapiro.test(log1p(dbo01))
shapiro.test(dbo01^(1/3))
#histogramas


hist(dbotab[,3],breaks=100,col="green")

tiff("histDBO.tiff",units="in",width=6,height=7,res=200)
hist(dbotab[,3]^(1/3),breaks=50,col="green",xlab="",main="DBO t/ano")
dev.off()

hist(log1p(dbotab[,3]),breaks=50,col="green")

tiff("histDBOconc1.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(dbotab[,4]^(1/3),breaks=40,col="green",main="DBO JAN",xlab="")
hist(dbotab[,5]^(1/3),breaks=40,col="green",main="DBO FEV",xlab="")
hist(dbotab[,6]^(1/3),breaks=40,col="green",main="DBO MAR",xlab="")
hist(dbotab[,7]^(1/3),breaks=40,col="green",main="DBO ABR",xlab="")
hist(dbotab[,8]^(1/3),breaks=40,col="green",main="DBO MAIO",xlab="")
hist(dbotab[,9]^(1/3),breaks=40,col="green",main="DBO JUN",xlab="")
dev.off()

tiff("histDBOconc2.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(dbotab[,10]^(1/3),breaks=40,col="green",main="DBO JUL",xlab="")
hist(dbotab[,11]^(1/3),breaks=40,col="green",main="DBO AGO",xlab="")
hist(dbotab[,12]^(1/3),breaks=40,col="green",main="DBO SET",xlab="")
hist(dbotab[,13]^(1/3),breaks=40,col="green",main="DBO OUT",xlab="")
hist(dbotab[,14]^(1/3),breaks=40,col="green",main="DBO NOV",xlab="")
hist(dbotab[,15]^(1/3),breaks=40,col="green",main="DBO DEZ",xlab="")
dev.off()

# Análise de componentes principais ---------------------------------------------

# i) para os dados númericos, sem transformação de distribuição------------------ 

pca5DF <-data.frame(dbotab[,1:15],Número.de.Indústrias,Metros)
pca5 <-prcomp(pca5DF,scale=T,center=T)

summary(pca5)

tiff("pca5.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca5,xlab="PC1(70%)",ylab="PC2(9%)")
biplot(pca5,choices=c(1,3),xlab="PC1(70%)",ylab="PC2(6%)")
dev.off()

scores.pca5 <- data.frame(Setor_industrial,pca5$x[,1:3])

pca5.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca5, 
	colour=factor(Setor_industrial),
	xlab="PC1(70%)", ylab="PC2(9%)") + 
  	theme(legend.position="right")

tiff("pca5.pc1.2",units="in",width=8,height=6,res=200)
pca5.pc1.2
	dev.off()

pca5.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca5, 
	colour=factor(Setor_industrial),
  	xlab="PC1(70%)",ylab="PC3(9%)")+
        theme(legend.position="right") 

tiff("pca5.pc1.3",units="in",width=8,height=6,res=200)
pca5.pc1.3
	dev.off()

# ii) Par os dados númericos, com tranformação cúbica----------------------------

pca6DF <-data.frame(dbotab[,1:2],(dbotab[,3:15]^(1/3)),Número.de.Indústrias,Metros)

pca6 <-prcomp(pca6DF,scale=T,center=T)
summary(pca6)

tiff("pca6.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca6,xlab="PC1(74%)",ylab="PC2(9%)")
biplot(pca6,choices=c(1,3),xlab="PC1(74%)",ylab="PC2(6%)")
dev.off()

scores.pca6 <- data.frame(Setor_industrial,pca6$x[,1:3])

pca6.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca6, 
	colour=factor(Setor_industrial),
	xlab="PC1(74%) ", ylab="PC2(10%)") + 
  	theme(legend.position="right")
 
tiff("pca6.pc1.2",units="in",width=8,height=6,res=200)
	pca6.pc1.2 
	dev.off()

pca6.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca6, 
	colour=factor(Setor_industrial),
         xlab="PC1(74%) ", ylab="PC2(10%)")+
  	theme(legend.position="right") 

tiff("pca6.pc1.3",units="in",width=8,height=6,res=200)
       pca6.pc1.3
     dev.off()

     detach(dbotab)
#---------------------------------------------------------------------------------

# ststab ------------------------------------------------------------------------

attach(ststab) 
summary(ststab)
dim(ststab) #dimensões da matriz (linhasXcolunas) 


#Plots de distribuição------------------------------------------------------------

tiff("histSTS.tiff",units="in",width=6,height=7,res=200)
hist(ststab[,3]^(1/3),breaks=50,col="green",xlab="",main="STS t/ano")
dev.off()

tiff("stsbwplot.tiff",units="in",width=11,height=6,res=200)
boxplot(ststab[,4:15],ylab="STS mg/L")	
dev.off()

tiff("stsbwplotlog.tiff",units="in",width=11,height=6,res=200)
boxplot(log(ststab[,4:15]),ylab="log STS mg/L") # melhor
dev.off()

tiff("stsbwplotcubic.tiff",units="in",width=11,height=6,res=200)
boxplot(ststab[,4:15]^(1/3),ylab="STS raíz cúbica mg/L")	
dev.off()

# testes de normalidade----------------------------------------------------------

# concentração de janeiro, representando a distribuição das outras---------------
qqnorm(sts01)
qqnorm(log1p(sts01))
qqnorm(sts01^(1/3))
shapiro.test(sts01)
shapiro.test(log1p(sts01))
shapiro.test(sts01^(1/3))

#histograms

tiff("histSTSconc1.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(ststab[,4]^(1/3),breaks=40,col="green",main="STS JAN",xlab="")
hist(ststab[,5]^(1/3),breaks=40,col="green",main="STS FEV",xlab="")
hist(ststab[,6]^(1/3),breaks=40,col="green",main="STS MAR",xlab="")
hist(ststab[,7]^(1/3),breaks=40,col="green",main="STS ABR",xlab="")
hist(ststab[,8]^(1/3),breaks=40,col="green",main="STS MAIO",xlab="")
hist(ststab[,9]^(1/3),breaks=40,col="green",main="STS JUN",xlab="")
dev.off()

tiff("histSTSconc2.tiff",units="in",width=10,height=10,res=200)
par(mfrow=c(2,3))
hist(ststab[,10]^(1/3),breaks=40,col="green",main="STS JUL",xlab="")
hist(ststab[,11]^(1/3),breaks=40,col="green",main="STS AGO",xlab="")
hist(ststab[,12]^(1/3),breaks=40,col="green",main="STS SET",xlab="")
hist(ststab[,13]^(1/3),breaks=40,col="green",main="STS OUT",xlab="")
hist(ststab[,14]^(1/3),breaks=40,col="green",main="STS NOV",xlab="")
hist(ststab[,15]^(1/3),breaks=40,col="green",main="STS DEZ",xlab="")
dev.off()

# Análise de componentes principais ---------------------------------------------

# i) para os dados númericos, sem transformação de distribuição------------------ 

pca7DF <-data.frame(ststab[,1:15],Número.de.Indústrias.sts,Metros.sts)
pca7 <-prcomp(pca7DF,scale=T,center=T)

summary(pca7)

tiff("pca7.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca7,xlab="PC1(43%)",ylab="PC2(17%)")
biplot(pca7,choices=c(1,3),xlab="PC1(43%)",ylab="PC3(13%)")
dev.off()

scores.pca7 <- data.frame(Setor.Industrial.sts,pca7$x[,1:3])

pca7.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca7, 
	colour=factor(Setor.Industrial.sts),
	xlab="PC1(43%)", ylab="PC2(17%)") + 
  	theme(legend.position="right")

tiff("pca7.pc1.2",units="in",width=8,height=6,res=200)
 pca7.pc1.2
dev.off()
	
pca7.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca7, 
	colour=factor(Setor.Industrial.sts),
        xlab="PC1 (43%)", ylab="PC3(13%)")+
  	theme(legend.position="right") 

tiff("pca7.pc1.3",units="in",width=8,height=6,res=200)
pca7.pc1.3
dev.off()

# ii) Par os dados númericos, com tranformação log--------------------------------

pca8DF <-data.frame(ststab[,1:2],(ststab[,3:15]^(1/3)),Número.de.Indústrias.sts,Metros.sts)

pca8 <-prcomp(pca8DF,scale=T,center=T)
summary(pca8)

tiff("pca8.biplot",units="in",width=8,height=6,res=200)
par(mfrow=c(1,2))
biplot(pca8,xlab="PC1(50%)",ylab="PC2(14%)")
biplot(pca8,choices=c(1,3),xlab="PC1(50%)",ylab="PC3(11%)")
dev.off()

scores.pca8 <- data.frame(Setor.Industrial.sts,pca8$x[,1:3])

pca8.pc1.2 <- qplot(x=PC1, y=PC2, data=scores.pca8, 
	colour=factor(Setor.Industrial.sts),
	xlab="PC1(50%)", ylab="PC2(14%)") + 
  	theme(legend.position="right") 

tiff("pca8.pc1.2",units="in",width=8,height=6,res=200)
pca8.pc1.2
dev.off()	

pca8.pc1.3 <- qplot(x=PC1, y=PC3, data=scores.pca8, 
	colour=factor(Setor.Industrial.sts),
        xlab="PC1(50%)", ylab="PC2(11%)")+
  	theme(legend.position="right")

tiff("pca8.pc1.3",units="in",width=8,height=6,res=200)
pca8.pc1.3
dev.off()
	
detach(ststab)

#=================================================================================

# 5. Testes de tendência temporal nos dados--------------------------------------

# criando data frames para as concentrações 

# para matab ---------------------------------------------------------------------

attach(matab)

concentraçõesMA <-c(ma01,ma02,ma03,ma04,ma05,ma06,ma07,ma08,ma09,ma10,ma11,ma12)

Jan <-rep("jan",length(ma01))
Fev <-rep("fev",length(ma02))
Mar <-rep("mar",length(ma03))
Abr <-rep("abr",length(ma04))
Maio <-rep("maio",length(ma05))
Jun<-rep("jun",length(ma06))
Jul <-rep("jul",length(ma07))
Ago <-rep("ago",length(ma08))
Set <-rep("set",length(ma09))
Out <-rep("out",length(ma10))
Nov <-rep("nov",length(ma11))
Dez <-rep("dez",length(ma12))

Mês.F <-c(Jan,Fev,Mar,Abr,Maio,Jun,Jul,Ago,Set,Out,Nov,Dez)

Verão <-rep("Verão",3*length(ma01))
Outono <-rep("Outono",3*length(ma01))
Inverno<-rep("Inverno",3*length(ma01))
Primavera<-rep("Primavera",3*length(ma01))

Estaçoes <-c(Verão,Outono,Inverno,Primavera)

matTemp <-data.frame(Mês.F,Estaçoes,concentraçõesMA)

# análises----------------------------------------------------------------------

plot(Mês,concentraçõesMA)
plot(log(concentraçõesMA)~Mês)
plot((concentraçõesMA)^(1/3)~Mês)


fit01 <-aov(concentraçõesMA~Mês.F)
summary(fit01)
coefficients(fit01)
TukeyHSD(fit01)
plot(TukeyHSD(fit01))

fit02 <-aov(concentraçõesMA~Estaçoes)
summary(fit02)
TukeyHSD(fit02) 
plot(TukeyHSD(fit02)) # não se pode descartar a possibilidade da igualdade
#entre verão e primavera


fit04 <-aov((concentraçõesMA)^(1/3)~Estaçoes) # melhoe, mas mais complexo
summary(fit04)
TukeyHSD(fit04)
plot(TukeyHSD(fit04))
coefficients(fit04)


detach(matab)

bwplot(concentraçõesMA~Estaçoes)
bwplot(concentraçõesMA^(1/3)~Estaçoes)
boxplot(concentraçõesMA^(1/3)~Estaçoes)
boxplot(concentraçõesMA^(1/3)~Mês.F)

#=================================================================================
# meses
# primavera (dezembro) 
# verão (fev)
# outono (abril)
# inverno (julho)
#=================================================================================

# para tatab------------------------------------------------------------------

attach(tatab)

concentraçõesTA <-c(ta01,ta02,ta03,ta04,ta05,ta06,ta07,ta08,ta09,ta10,ta11,ta12)

Jan <-rep("jan",length(ta01))
Fev <-rep("fev",length(ta02))
Mar <-rep("mar",length(ta03))
Abr <-rep("abr",length(ta04))
Maio <-rep("maio",length(ta05))
Jun<-rep("jun",length(ta06))
Jul <-rep("jul",length(ta07))
Ago <-rep("ago",length(ta08))
Set <-rep("set",length(ta09))
Out <-rep("out",length(ta10))
Nov <-rep("nov",length(ta11))
Dez <-rep("dez",length(ta12))

Mês.F <-c(Jan,Fev,Mar,Abr,Maio,Jun,Jul,Ago,Set,Out,Nov,Dez)

Verão <-rep("Verão",3*length(ta01))
Outono <-rep("Outono",3*length(ta01))
Inverno<-rep("Inverno",3*length(ta01))
Primavera<-rep("Primavera",3*length(ta01))

Estaçoes <-c(Verão,Outono,Inverno,Primavera)

taTemp <-data.frame(concentraçõesTA,Mês.F,Estaçoes)

fit04 <-aov(concentraçõesTA~Mês.F)
summary(fit04)
coefficients(fit04)
TukeyHSD(fit04)
plot(TukeyHSD(fit04)) # nenhuma diferença significativa

fit05 <-aov(concentraçõesTA~Estaçoes)
summary(fit05)
plot(TukeyHSD(fit05))
TukeyHSD(fit05)
coefficiets(fit05)


fit051 <-aov(concentraçõesTA^(1/3)~Estaçoes)# melhor 
summary(fit051)
TukeyHSD(fit051)
plot(TukeyHSD(fit051)) # verão-outono e verão-primavera

bwplot(concentraçõesTA~Estaçoes)
bwplot(concentraçõesTA^(1/3)~Estaçoes)
boxplot(concentraçõesTA^(1/3)~Estaçoes)
boxplot(concentraçõesTA^(1/3)~Mês.F)

detach(tatab)

# para dbotab----------------------------------------------------------------

attach(dbotab)

concentraçõesDBO<-c(dbo01,dbo02,dbo03,dbo04,dbo05,dbo06,dbo07,dbo08,dbo09,dbo10,dbo11,dbo12)

Jan <-rep("jan",length(dbo01))
Fev <-rep("fev",length(dbo01))
Mar <-rep("mar",length(dbo01))
Abr <-rep("abr",length(dbo01))
Maio <-rep("maio",length(dbo01))
Jun<-rep("jun",length(dbo01))
Jul <-rep("jul",length(dbo01))
Ago <-rep("ago",length(dbo01))
Set <-rep("set",length(dbo01))
Out <-rep("out",length(dbo01))
Nov <-rep("nov",length(dbo01))
Dez <-rep("dez",length(dbo01))

Mês.F <-c(Jan,Fev,Mar,Abr,Maio,Jun,Jul,Ago,Set,Out,Nov,Dez)

Verão <-rep("Verão",3*length(dbo01))
Outono <-rep("Outono",3*length(dbo01))
Inverno<-rep("Inverno",3*length(dbo01))
Primavera<-rep("Primavera",3*length(dbo01))

Estaçoes <-c(Verão,Outono,Inverno,Primavera)

dboTemp <-data.frame(concentraçõesDBO,Mês.F,Estaçoes)

fit06 <-aov(concentraçõesDBO~Mês.F)
summary(fit06)
coefficients(fit06)
TukeyHSD(fit06)
plot(TukeyHSD(fit06))

fit07 <-aov(concentraçõesDBO~Estaçoes) # melhor 
summary(fit07)
plot(TukeyHSD(fit07))
TukeyHSD(fit07)

fit071 <-aov((concentraçõesDBO^3)~Estaçoes)
summary(fit071)
plot(TukeyHSD(fit071))
TukeyHSD(fit071)

detach(dbotab)

bwplot(concentraçõesDBO~Estaçoes)


# para ststab-----------------------------------------------------------------

attach(ststab)

concentraçõesSTS <-c(sts01,sts02,sts03,sts04,sts05,sts06,sts07,sts08,sts09,sts10,sts11,sts12)

Jan <-rep("jan",length(sts01))
Fev <-rep("fev",length(sts01))
Mar <-rep("mar",length(sts01))
Abr <-rep("abr",length(sts01))
Maio <-rep("maio",length(sts01))
Jun<-rep("jun",length(sts01))
Jul <-rep("jul",length(sts01))
Ago <-rep("ago",length(sts01))
Set <-rep("set",length(sts01))
Out <-rep("out",length(sts01))
Nov <-rep("nov",length(sts01))
Dez <-rep("dez",length(sts01))

Mês.F <-c(Jan,Fev,Mar,Abr,Maio,Jun,Jul,Ago,Set,Out,Nov,Dez)

Verão <-rep("Verão",3*length(sts01))
Outono <-rep("Outono",3*length(sts01))
Inverno<-rep("Inverno",3*length(sts01))
Primavera<-rep("Primavera",3*length(sts01))

Estaçoes <-c(Verão,Outono,Inverno,Primavera)

stsTemp <-data.frame(concentraçõesSTS,Mês.F,Estaçoes)

fit08 <-aov(concentraçõesSTS~Mês.F)
summary(fit08)
coefficients(fit08)
fit09 <-aov(concentraçõesSTS~Estaçoes)
summary(fit09)
TukeyHSD(fit09)

fit091 <-aov(concentraçõesSTS^(1/3)~Estaçoes) # melhor 
summary(fit091)
TukeyHSD(fit091)

bwplot(concentraçõesDBO~Estaçoes)

detach(ststab)

#============================================================================

# 6. Análise de distribuição espacial----------------------------------------

## para dados de MA----------------------------------------------------------

# para emissões (tonelada por ano)
tiff("map1.tiff", width = 6, height = 6, units = 'in', res = 200)
EmissionMap(data=matab,y=3,dim=2,title="MA t/ano",b=2,e=0.5) # emissão por ano 
dev.off()

# representando o as estações de primavera----------------------------
tiff("map4.tiff", width = 6, height = 6, units = 'in', res = 200)
EmissionMap(data=matab,y=15,dim=3,title="MA (mg/L) Dezembro",b=1,e=0.5) # concentração de dezembro  
dev.off()

# representando o as estações de verão--------------------------------------------
tiff("map6.tiff", width = 6, height = 6, units = 'in', res = 200)
EmissionMap(data=matab,y=5,b=1,dim=3,title="MA (mg/L) Fevereiro",e=0.5) # concentração de feveriro
dev.off()

# representando o as estações de outono-------------------------------------------
tiff("map8.tiff", width = 6, height = 6, units = 'in', res = 200)
EmissionMap(data=matab,y=7,dim=3,b=1,title="MA (mg/L) Abril",e=0.5) # concentração de março
dev.off()

# representando o as estações de Inverno----------------------------------------
tiff("map10.tiff", width = 6, height = 6, units = 'in', res = 200)
EmissionMap(data=matab,b=1,y=10,dim=3,title="MA (mg/L) Julho",e=0.5) # concentração de maio
dev.off()

# Mapas para tatab----------------------------------------------------------------

tiff("map12.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=tatab,y=3,dim=3,title="TA t/ano",e=0.2,b=0,n=3) # emissão por ano 
dev.off()

# primavera----------------------------------------------------------------------
tiff("map14.tiff", width = 6, height = 5, units = 'in', res = 200,)
EmissionMap(data=tatab,y=15,dim=3,title="TA (mg/L) Dezembro",e=0.2,b=3,n=3) # concentração de feveriro
dev.off()

# verão----------------------------------------------------------------------
tiff("map16.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=tatab,y=5,dim=3,title="TA (mg/L) Fevereiro",e=0.2,b=3,n=3) # concentração de feveriro
dev.off()

# outono----------------------------------------------------------------------
tiff("map18.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=tatab,y=7,dim=3,title="TA (mg/L) Abril",e=0.2,b=3,n=3) # concentração de feveriro
dev.off()

# inverno----------------------------------------------------------------------
tiff("map19.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=tatab,y=10,dim=3,title="TA (mg/L) Julho",e=0.2,b=3,n=3) # concentração de feveriro
dev.off()

# mapas para os dados de emissão de dbotab----------------------------------------

tiff("map21.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=dbotab,y=3,dim=2,title="DBO t/ano",e=0.5,b=0,n=2) # emissão por ano 
dev.off()

# Primavera----------------------------------------------------------------------
tiff("map23.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=dbotab,y=15,dim=3,title="DBO (mg/L) Dezembro",e=0.2,b=3,n=2) # concentração de janeiro 
dev.off()

# verão----------------------------------------------------------------------
tiff("map25.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=dbotab,y=5,dim=3,title="DBO (mg/L) Fevereiro",e=0.5,b=3,n=2) # concentração de feveriro
dev.off()

# outono----------------------------------------------------------------------
tiff("map27.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=dbotab,y=7,dim=3,title="DBO (mg/L) Abril",e=0.5,b=3,n=2) # concentração de feveriro
dev.off()

# inverno----------------------------------------------------------------------
tiff("map29.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=dbotab,y=10,dim=3,title="DBO (mg/L) Julho",e=0.5,b=3,n=2) # concentração de feveriro
dev.off()

# mapas para ststab-------------------------------------------------------------

tiff("map31.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=ststab,y=3,dim=2,title="STS t/ano",b=0,e=0.5,n=2) # emissão por ano 
dev.off()

# Primavera----------------------------------------------------------------------
tiff("map33.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=ststab,y=14,dim=3,title="STS (mg/L) Dezembro",b=3,e=0.5,n=2) # concentração de janeiro 
dev.off()

# verão----------------------------------------------------------------------
tiff("map35.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=ststab,y=5,dim=3,title="STS (mg/L) Fevereiro",b=3,e=0.5) # concentração de feveriro
dev.off()

# outono----------------------------------------------------------------------
tiff("map37.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=ststab,y=7,dim=2,title="STS (mg/L) Abril",b=3,e=0.4) # concentração de feveriro
dev.off()

# inverno----------------------------------------------------------------------
tiff("map39.tiff", width = 6, height = 5, units = 'in', res = 200)
EmissionMap(data=ststab,y=10,dim=3,title="STS (mg/L) Julho",b=3,e=0.5) # concentração de feveriro
dev.off()

#================================================================================

# 8. classificação para as classes do Conama-------------------------------------


con1 <-conamaClassMetais(data=matab,y=10)

conamaClassDBO(data=dbotab,y=1)


# representando o as estações de primavera---------------------------------------
tiff("map41.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=matab,y=15,title="Classe Conama em Dezembro (metais)",PP="MA",e=0.5) 
dev.off()

# representando o as estações de verão--------------------------------------------
tiff("map42.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=matab,y=5,title="Classe Conama em Fevereiro (metais)",PP="MA",e=0.5) 
dev.off()

# representando o as estações de outono-------------------------------------------
tiff("map43.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=matab,y=7,title="Classe Conama em Abril (metais)",PP="MA",e=0.5) 
dev.off()

# representando o as estações de Inverno----------------------------------------
tiff("map44.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=matab,y=10,title="Classe Conama em Julho (metais)",PP="MA",e=0.5) 
dev.off()

# primavera----------------------------------------------------------------------
tiff("map45.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=tatab,y=15,title="Classe Conama em Dezembro (Tóxicos)",PP="TA",e=0.4) 
dev.off()

# verão----------------------------------------------------------------------
tiff("map46.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=tatab,y=5,title="Classe Conama em Fevereiro (Tóxicos)",PP="TA",e=0.2) 
dev.off()

# outono----------------------------------------------------------------------
tiff("map47.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=tatab,y=7,title="Classe Conama em Abril (Tóxicos)",PP="TA",e=0.2) 
dev.off()

# inverno----------------------------------------------------------------------
tiff("map48.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=tatab,y=10,title="Classe Conama em Julho (Tóxicos)",PP="TA",e=0.2) 
dev.off()

# Primavera----------------------------------------------------------------------
tiff("map49.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=dbotab,y=15,title="Classe Conama em Dezembro (DBO)",e=0.2,PP="DBO") 
dev.off()

# verão----------------------------------------------------------------------
tiff("map50.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=dbotab,y=5,title="Classe Conama em Fevereiro (DBO)",e=0.5,PP="DBO") 
dev.off()

# outono----------------------------------------------------------------------
tiff("map51.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=dbotab,y=7,title="Classe Conama em Abril (DBO)",e=0.5,PP="DBO") 
dev.off()

# inverno----------------------------------------------------------------------
tiff("map52.tiff", width = 6, height = 5, units = 'in', res = 200)
conamaClass(Data=dbotab,y=11,title="Classe Conama em Julho (DBO)",e=0.5,PP="DBO") 
dev.off()
--------------------------------------------------------------

#==============================================================

#9. Biplots de PCA --------------------------------------------

tiff("pca1.biplot1",units="in",width=6,height=6,res=200)
biplot(pca1,xlab="PC1(60%)",ylab="PC2(10%)")
dev.off()

tiff("pca1.biplot2",units="in",width=6,height=6,res=200)
biplot(pca1,choices=c(1,3),xlab="PC1(60%)",ylab="PC3(6%)")
dev.off()

tiff("pca2.biplot1",units="in",width=6,height=6,res=200)
biplot(pca2,xlab="PC1(62%)",ylab="PC2(10%)")
dev.off()

tiff("pca2.biplot2",units="in",width=6,height=6,res=200)
biplot(pca1,choices=c(1,3),xlab="PC1(60%)",ylab="PC3(6%)")
dev.off()


tiff("pca3.biplot1",units="in",width=6,height=6,res=200)
biplot(pca3,xlab="PC1(60%)",ylab="PC2(10%)")
dev.off()

tiff("pca3.biplot2",units="in",width=6,height=6,res=200)
biplot(pca3,choices=c(1,3),xlab="PC1(60%)",ylab="PC3(6%)")
dev.off()

tiff("pca4.biplot1",units="in",width=6,height=6,res=200)
biplot(pca4,xlab="PC1(62%)",ylab="PC2(10%)")
dev.off()

tiff("pca4.biplot2",units="in",width=6,height=6,res=200)
biplot(pca4,choices=c(1,3),xlab="PC1(62%)",ylab="PC3(7%)")
dev.off()

tiff("pca5.biplot1",units="in",width=6,height=6,res=200)
biplot(pca5,xlab="PC1(70%)",ylab="PC2(9%)")
dev.off()

tiff("pca5.biplot2",units="in",width=6,height=6,res=200)
biplot(pca5,choices=c(1,3),xlab="PC1(70%)",ylab="PC3(6%)")
dev.off()

tiff("pca6.biplot1",units="in",width=6,height=6,res=200)
biplot(pca6,xlab="PC1(74%)",ylab="PC2(9%)")
dev.off()

tiff("pca6.biplot2",units="in",width=6,height=6,res=200)
biplot(pca6,choices=c(1,3),xlab="PC1(74%)",ylab="PC3(6%)")
dev.off()

tiff("pca7.biplot1",units="in",width=6,height=6,res=200)
biplot(pca7,xlab="PC1(43%)",ylab="PC2(17%)",ylim=c(-0.3,0.35),xlim=c(-0.45,0.3))
dev.off()

tiff("pca7.biplot2",units="in",width=6,height=6,res=200)
biplot(pca7,choices=c(1,3),xlab="PC1(43%)",ylab="PC3(13%)",ylim=c(-0.36,0.34),xlim=c(-0.5,0.4))
dev.off()

tiff("pca8.biplot1",units="in",width=6,height=6,res=200)
biplot(pca8,xlab="PC1(50%)",ylab="PC2(14%)")
dev.off()

tiff("pca8.biplot2",units="in",width=6,height=6,res=200)
biplot(pca8,choices=c(1,3),xlab="PC1(50%)",ylab="PC3(11%)")
dev.off()


