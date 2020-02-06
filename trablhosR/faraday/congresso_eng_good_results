#investigation_engrand congress, good results

# continue the investigation for get the conclusions and make the first models

# 0-packages 
library("pls")
library("lattice")

# 1-Tables

ionic1 <-read.table("r/ionic", header=T, row.names=1)
# separate 

ionic2 <-subset(ionic1,Co2.sol!=0)
znbrm <-subset(ionic2,znbr==1)
ntf2 <-subset(ionic2,a.radii==4.39)
ionicp <-subset(ionic2,a.radii!=4.39)
ionic3 <-subset(ionic2,select2=="no")

#biplots

tsel <-exp(-select/256)
tyie <-exp(-Yield/256)

#pode ser usado para fazer o modelo de Yield em função de Co2.sol;znbr data set 
xyplot(tyie~Co2.sol,data=znbrm,auto.key=T)



#base para fazer modelo de TON e TOF em função das condições da reação; znbr data set
xyplot(TON~exp((-Time*Tem*Pr)/5000),data=znbrm,auto.key=T,groups=Tem)
xyplot(TOF~exp((-Time*Tem*Pr)/5000),data=znbrm,auto.key=T,groups=Tem)

# para ntf2 data set 
xyplot(TON~Time,data=ntf2,auto.key=T)
#boa para modelo em função das condições
xyplot(TON~Time,data=ntf2,auto.key=T,groups=znbr)

# boa para ionicp em função para CO2 para yield
xyplot(select+Yield~Co2.sol,data=ionicp,auto.key=T)

#modelos para ionic properties; fazer super plot para mostrar as correlações
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
xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~TON,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~Yield,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~select,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

xyplot(100*a.radii+mw+MP+10*Co2.sol+tg+100*surface~tyie,data=ionic3,auto.key=T,groups=znbr,pch=0:1)

#yield model 
pls1 <-mvr(Yield~(znbr*mw)+MP+(log(tg))+Co2.sol/mw,3,data=ionic3,validation="LOO",method="oscorespls")

zmw <-(mw*znbr)


pls1 <-mvr(Yield~zmw+MP+Co2.sol/mw,3,data=ionic3,validation="LOO",method="oscorespls")


pls1 <-mvr(Yield~zmw+MP+Co2.sol,3,data=ionic3,validation="LOO",method="oscorespls")


pls1 <-mvr(Yield~zmw+MP+Co2.sol/mw,3,data=ionic4,validation="LOO",method="oscorespls")










