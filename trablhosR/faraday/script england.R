# Poster para congresso para a inglaterra 

# Summary
# 1-Tables
# 2-PCA
# 3-Biplots and linearizations
# 3.1- Best results of the biplots
# 4- PCA para diferenças dos os IL 
# 5- first PLS models
# 6- plots para trends ionicos 
# 7- modelos para o poster 
# 8- gráficos para o poster 

# 0-packages 
library("pls")
library("lattice")

# 1-Tables
setwd("~/r/faraday")
ionic1 <-read.table("ionic", header=T, row.names=1)

yield.mod1 <-subset(ionic1,yield1=="yes")[,1:20]
yield.mod2 <-subset(ionic1,yield2=="yes")[,1:20]
selec.mod1 <-subset(ionic1,select1=="yes")[,1:20]
selec.mod2 <-subset(ionic1,select2=="yes")[,1:20]
ionic2 <-subset(ionic1,Co2.sol!=0)
ionic3 <-subset(ionic2,select2=="no")

# 2-Statistics and PCA

summary(ionic1)
summary(yield.mod1)
summary(yield.mod2)
summary(selec.mod1)
summary(selec.mod2)


pcionic <-prcomp(ionic1[,1:16])
summary(pcionic)
loadings(pcionic)
pcyield1 <-prcomp(yield.mod1[,1:16])
summary(pcyield1)
loadings(pcyield1)
pcyield2 <-prcomp(yield.mod2[,1:16])
summary(pcyield2)
loadings(pcyield2)
pcselec <-prcomp(selec.mod1[,1:16])
summary(pcselec)
loadings(pcselec)
pcselec2 <-prcomp(selec.mod2[,1:16])
summary(pcselec2)
loadings(pcselec2)

cor(ionic1[,1:16])
cor(yield.mod1[,1:16])
cor(yield.mod2[,1:16])
cor(selec.mod1[,1:16])
cor(selec.mod2[,1:16])

#correlation between responses

attach(ionic1)
lmod1 <-lm(Yield~select)
summary(lmod1)
detach(ionic1)
attach(yield.mod1)
lmod2 <-lm(Yield~select)
summary(lmod2)
detach(yield.mod1)
attach(yield.mod2)
lmod3 <-lm(Yield~select)
summary(lmod3)
detach(yield.mod2)

# 3-biplots
attach(ionic1)
# bivariate plots for ionic 
# key for the plots
key1 <-list(space="right",text=list(‌names(a.radii)),points=list(pch=1:5,col="black"))
#1 pattern between pressure and selectivity


#2 pattern between anion radii and yield activated with znbr
xyplot(Yield+TON+TOF+select~a.radii,data=ionic1,groups=znbr,auto.key=T)
#3 ZnBR2 effect with melting point
xyplot(Yield+TON+TOF+select~MP,data=ionic1,groups=znbr,auto.key=T)
#4 correlation of surface tension with ZnBR2 effect
xyplot(Yield+TON+TOF+select~surface,data=ionic1,groups=znbr,auto.key=T)
#5 pattern between mw with yield
xyplot(Yield+TON+TOF+select~mw,data=ionic1,groups=znbr,auto.key=T)
#6 possible cross effect, znbr could cause contrary effects in respect  to zncl
xyplot(Yield+TON+TOF+select~Co2.sol,data=ionic1,groups=znbr,auto.key=T)
#7 effect of zncl and znbr with mw
xyplot(Yield~a.radii+Dipty+mw+surface+Co2.sol|znbr,data=yield.mod1,auto.key=T)
#8 intersting pattern with logof surface tension and Yield
xyplot(Yield~log(surface),data=yield.mod1,type=c("g","p"),auto.key=T)
xyplot(Yield~log(surface),data=yield.mod1,groups=znbr,type=c("g","p"),auto.key=T)
xyplot(Yield~log(surface),data=yield.mod1,groups=zncl,type=c("g","p"),auto.key=T)
#9 A curve is drawn for each cocat
xyplot(Yield~MP^2.718,data=yield.mod1,type=c("g","p"),groups=zncl,auto.key=T)
#10 negative pattern bwtween co2.sol (figure1)(tirar o sem il e investigar com os outrosgrupos
xyplot(Yield~Co2.sol,data=yield.mod1,type=c("g","p"),groups=znbr,auto.key=T)
xyplot(Yield~(Co2.sol*Pr),data=yield.mod1,type=c("g","p"),groups=Pr,auto.key=T)
# 11 mw with the different cocat
xyplot(mw~Yield|znbr,data=yield.mod1[1:10,],type=c("g","p"),groups=znbr)
# 12
xyplot(select~tg,data=selec.mod1,type=c("g","p"))
# cross effect
xyplot(select~Co2.sol^2,data=selec.mod1,type=c("g","p"),groups=znbr)
# 14
xyplot(select~Tem,data=selec.mod2,type=c("g","p"),groups=Pr)
# 15 same patterns found earlier
xyplot(TON~Co2.sol,data=ionic1,type=c("g","p"),groups=znbr)
## 16 same patterns as yield
xyplot(TON^2~MP,data=yield.mod1,type=c("g","p"))

#17 investigation between pressure and selectivity
attach(ionic1)
xyplot(select~Pr,data=ionic1,groups=a.radii,auto.key=T,pch=1:5)
xyplot(select~Pr|znbr,data=ionic1,auto.key=T)

xyplot(select~Pr|znbr,data=ionic1,groups=Tem,auto.key=T) 

# 18 biplot sem os cocatalyst puros

xyplot(select~Pr,data=ionic2,groups=a.radii,auto.key=T,pch=1:5)
xyplot(select~Pr|znbr,data=ionic2,auto.key=T)
xyplot(select~Pr|znbr,data=ionic1,groups=mw,auto.key=T)

# 19 NTF2 tem boa seletividade e rendimento em todas as reações que tem znbr2, idenpendente das condições de temperatura e tempo, mas com alguma dependencia em função da pressão. 
ionic2 <-subset(ionic1, Co2.sol!=0)
ionic3 <-subset(ionic2,znbr!=0)
xyplot(Yield+select~Pr,data=ionic3,auto.key=T,groups=Co2.sol)

#3.1 better results for the invesgation

#investigar; interessante
xyplot(tsel+tyie~mw,data=ionicp,auto.key=T)
#investigar; interessante 2 
xyplot(tsel+tyie~tg,data=ionicp,auto.key=T)
#investigar; interessante 3 
xyplot(TON+TOF~tg,data=ionicp,auto.key=T)
#not much
xyplot(TON+TOF~Tem,data=ionicp,auto.key=T)
#interessante 4
xyplot(TON+TOF~Co2.sol,data=ionicp,auto.key=T)
#nada
xyplot(TON+TOF~Pr,data=ionicpntf2,auto.key=T)
#interessante 5
xyplot(TON+TOF~a.radii,data=ionicp,auto.key=T)
#interessante 6
xyplot(TON+TOF~mw,data=ionicp,auto.key=T)
# interessante 7
xyplot(TON+TOF~MP,data=ionicp,auto.key=T)



#para ionic3 


#investigar; interessante
xyplot(tsel+tyie~mw,data=ionic3,auto.key=T)
#investigar; interessante 2 
xyplot(tsel+tyie~tg,data=ionic3,auto.key=T)
#investigar; interessante 3 
xyplot(TON+TOF~tg,data=ionic3,auto.key=T)

#+ ou -
xyplot(TON+TOF~Co2.sol,data=ionic3,auto.key=T)
#interessante 5
xyplot(TON+TOF~a.radii,data=ionic3,auto.key=T)
#interessante 6
xyplot(TON+TOF~mw,data=ionic3,auto.key=T)
# interessante 7
xyplot(TON+TOF~MP,data=ionic3,auto.key=T)

# ionic trends
xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~TON,data=ionic3,auto.key=T,groups=znbr,pch=0:1

# 4- Diferenças entre os LI 

difionic <- read.table("r/difionic",header=T,row.names=1)
#PCA 
em função das prorpiedades que mais variam de IL para  IL esse PCA mostra
que BF4 e CL são bem mais próximos em propriedades do que com o NTF2, o que poderia explicar o comportamente do NTF2 na catálise. Isso pode explicar a maior parte das incertezas dos modelos que contemplam os três tipos de íons, já que apresentam sistemas diferentes. 
pcadif <- prcomp(difionic)
biplot(pcadif)

#5 - PLS 

# 5.1 abordagem iônica
attach(yield.mod1)
#basis set tranformation

lTg <-log1p(tg)
ldip <-log1p(Dipty)
ltemp <-log(Tem)
ltime <-log(Time)
mznbr <- mw*znbr

#pls model para yield.mod1, tentar com resposta reciproca
#fail
pls1 <-mvr(Yield~mznbr+surface+MP+lTg,3,data=yield.mod1,validation="CV",scale=T, method="oscorespls")

# faill
pls <-mvr(Yield~mznbr+MP++Co2.sol,3,data=yield.mod1,validation="CV",method="oscorespls")

pls1 <-mvr(Yield~mznbr+surface+MP+lTg+Co2.sol,3,data=yield.mod1,validation="CV",method="oscorespls")

#R2  0.52208, fazer com objetos diferentes
pls1 <-mvr(Yield~mznbr+surface+MP+lTg+Co2.sol/mw,3,data=yield.mod1,validation="CV",method="oscorespls")

#fail
pls1 <-mvr(Yield~mznbr+MP+Co2.sol/mw+ldip,4,data=yield.mod1,validation="CV",method="oscorespls")

#mais ou menos, tentar melhorar
pls1 <-mvr(Yield~mznbr+surface+MP+lTg+Co2.sol/mw+Dipty,4,data=yield.mod1,validation="CV",method="oscorespls")

#fazer com objetos diferentes
pls1 <-mvr(Yield~mznbr+MP+lTg+(Co2.sol/mw),3,data=yield.mod1,validation="CV",method="oscorespls")

summary(pls1)
R2(pls1)
#modelo para seletividade

pls.selec1 <-mvr(select~znbr+tg+Dipty,3,data=yield.mod1,validation="LOO",method="oscorespls")

#modelo para yield.mod2
#lembrar de desanexar outras data.frame 
attach(yield.mod2)

lTg <-log1p(tg)
ldip <-log1p(Dipty)
ltemp <-log(Tem)
ltime <-log(Time)
mznbr <- mw*znbr

pls.yield2 <-mvr(Yield~temp+Pr+Co2.sol+ltime,3,data=yield.mod2,scale=T,validation="LOO",method="oscorespls")
biplot(pls.yield2)



#6 plots para ionic trends


A.Radii <-a.radii*100
xCO2 <-10*Co2.sol
Surf.Tension <-100*surface

xyplot(A.Radii+mw+MP+xCO2+tg+Surf.Tension~Yield,data=ionic3,auto.key=T,groups=znbr,pch=0:1,xlab="Yield",ylab="Ionic Features")

xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~Yield,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~select,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~tyie,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

#7 modelos para o poster 

## para Yield
#vatiaveis transformadas
lTg <-log1p(tg)
ldip <-log1p(Dipty)
ltemp <-log(Tem)
tsel <-exp(-select/256)
tyie <-exp(-Yield/256)
time <-log(Time)
mznbr <- mw*znbr
zMP <- znbr*MP
comw <- Co2.sol/mw
tem <-(Tem-390)^2
##não esquecer de usar data frame correto e de carregar a variaveis transmformadas
### melhor, suspeito mas esticamente bom; estudar as validações e transformações
attach(yield.mod1)
pls1 <-mvr(tyie~mznbr+MP+Co2.sol/mw,3,data=yield.mod1,validation="CV",method="oscorespls")

summary(pls1)
fitted.values(pls1)
residuals(pls1)

#resultados
# Data: 	X dimension: 12 4 
#	Y dimension: 12 1
#Fit method: oscorespls
#Number of components considered: 3

#VALIDATION: RMSEP
#Cross-validated using 10 random segments.
#       (Intercept)  1 comps  2 comps  3 comps
#CV          0.1133   0.1189  0.09899  0.06997
#adjCV       0.1133   0.1182  0.09868  0.06886

#TRAINING: % variance explained
#      1 comps  2 comps  3 comps
#X     99.9604    99.99   100.00
#tyie   0.2038    54.48    77.97

#fitted values for 3 comps
        tyie
#1  0.9660126
#2  0.9660126
#3  0.8244016
#4  0.7933099
#5  0.9446682
#6  0.7933099
#7  0.8683516
#8  0.7535843
#9  0.9908828
#10 1.0176996
#15 0.7520446
#16 0.8727878

#validação do primeiro modelo aceito utilizado no poster
vali.pls1 <-read.table("r/vali_pls1",header=T,row.names=1)
attach(vali.pls1)

sst <-sum((mean(fit.tyie)-fit.tyie)^2)
sse <-sum((res.tyie)^2)
R2 <-(1-(sse/sst))
#com graus de liberdade
R2 <-(1-((sse/11)/(sst/8)))
# R2 0,7944
coefficients(pls1,intercept=T)

#                     tyie
#(Intercept)  4.756398e-01
#mznbr       -3.376838e-04
#MP           9.266959e-04
#Co2.sol     -2.735774e-06
#Co2.sol:mw   1.061140e-05

#modelo final para Yield em função das condições

pls.yield2 <-mvr(tyie~tem+Pr+Co2.sol+time,3,data=yield.mod2,scale=T,validation="LOO",method="oscorespls")
summary(pls.yield2)
fitted.values(pls.yield2)
residuals(pls.yield2)

#results for the second model 

#Data: 	X dimension: 12 4 
#	Y dimension: 12 1
#Fit method: oscorespls
#Number of components considered: 3

#VALIDATION: RMSEP
#Cross-validated using 10 random segments.
#       (Intercept)  1 comps  2 comps  3 comps
#CV          0.1133   0.1189  0.09899  0.06997
#adjCV       0.1133   0.1182  0.09868  0.06886

#TRAINING: % variance explained
#      1 comps  2 comps  3 comps
#X     99.9604    99.99   100.00
#tyie   0.2038    54.48    77.97




coefficients(pls.yield2,intercept=T)
#, , 3 comps

           #         tyie
#(Intercept)  0.975453937
#tem         -0.008146679
#Pr          -0.002889085
#Co2.sol     -0.002862295
#ltime       -0.026752014


vali.pls1 <-read.table("r/vali_pls2",header=T,row.names=1)
attach(vali.pls2)

sst <-sum((mean(fit.tyie)-fit.tyie)^2)
sse <-sum((res.tyie)^2)


R2 <-(1-((sse/8)/(sst/4)))

# R2 0.8586983

# montagem do gráfico 3d

yield <-  0.975453937-0.008146679*(ltime)-0.002862295*(co2)-0.008146679*(t)-0.002889085*(Pr)



resppls2 <-read.table("r/resppls2", header=T)

pls2cur <-read.table("r/pls2cur",header=T)
attach(pls2cur)


z <-log(-yi.pls2)*(-256)

# 90.34007 86.60671 82.81383 79.18786 75.67284 72.24303

CO2v <-c(10,20,30,40,50,60)

Tomeg <-c(1.5,2,3,4,5,6)
lt <-log(tomeg)

> yi.pls2 <-interc+(teta*lt)+(CO2h*CO2v)+(bar*0.1)
> yi.pls2
#[1] 0.9432389 0.9122723 0.8803461 0.8493795 0.8189387 0.7888304
> log(yi.pls2)*(-256)
#[1] 14.95954 23.50510 32.62467 41.79178 51.13499 60.72420
yieldg <-c(14.95954,23.5051,32.6246,41.79178,51.1349,60.72420)

#gráficos para o poster 
# gráfico trends iônicos
#variáveis criadas para os gráficos 
attach(ionic3)
A.Radii <-a.radii*100
xCO2 <-10*Co2.sol
Surf.Tension <-100*surface
MW <-mw
Surface_Tension <-100*surface
Tg <-tg

#function for save graphs in jpeg
jpeg(filename = "ionic.jpeg",
          width = 480, height = 480, units = "px", pointsize = 12,
          quality = 100,
          bg = "white", res = NA, ...,
          type = c("cairo", "Xlib", "quartz"), antialias)


xyplot(A.Radii+MW+MP+xCO2+Tg+Surface_Tension~Yield,data=ionic3,auto.key=list(space="right"),groups=znbr,pch=0:1,xlab="Yield",ylab="",main="Ionic Features")
cocat <-c("without ZnBr2","ZnBr2")

key.ionic <-list(space="right",text=list(cocat))
#surface plots pls-r
#carregar variaveis e modelo


ionic1 <-read.table("r/ionic", header=T, row.names=1)
yield.mod2 <-subset(ionic1,yield2=="yes")[,1:20]

attach(yield.mod2)
tem <-(Tem-390)^2
tyie <-exp(-Yield/256)
time <-log(Time)



pls.yield2 <-mvr(tyie~tem+Pr+Co2.sol+time,3,data=yield.mod2,scale=T,validation="LOO",method="oscorespls")

#criar grid de variaveis para predizer com o modelo

time.mesh <- with(yield.mod2, do.breaks(range(time), 20))
temp.mesh <- with(yield.mod2, do.breaks(range(tem),20))
pr.mesh <- with(yield.mod2, do.breaks(range(Pr), 20))
co2.mesh <- with(yield.mod2, do.breaks(range(Co2.sol), 20))

grid <-
expand.grid(time= time.mesh,
tem = temp.mesh,
Pr = pr.mesh,
Co2.sol =co2.mesh)

grid[["Yield"]] <- (log(predict(pls.yield2, newdata = grid)))*(-256)
attach(grid)
wireframe(Yield~time*Co2.sol)
cloud(Yield~time+Co2.sol,data=grid)

grid[["Yield"]] <- predict(pls.yield2, newdata = grid)

pdf(file="r/img/pm34.pdf")
wireframe(Yield~time*Co2.sol,data=grid,color.key=T,shade=T,drape=T,xlab="log Time",ylab="xCO2")
dev.off()



    
