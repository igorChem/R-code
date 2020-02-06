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
ionic3 <-subset(ionic2,select2==no)

#histograms

attach(ionic2)
#excesso entre 50 e 60; relativamente alto número de zeros
hist(Yield)

#bem distribuído em volta da moda; excesso de zeros; excesso entre 50 e 60 
hist(TON)

# muitos zeros; bem distribuído entre 2 e 6
hist(TOF)

#excesso entre 80 e 100; poucas informações abaixo disso
hist(select)

detach(ionic2)
attach(znbrm)

#bem distribúido, com excesso em 50 e 60
hist(Yield)
#opção de correção com RBF; contante em 64 é interessante tm
hist(exp(-Yield/32))

#bem distribuído em torno da média; excesso entre 20 e 25
hist(TON)

#parece interessante
hist(exp(-TON/256))

#bem distribuído
hist(TOF)

#pode ser usado para homogenizar a distribuição
hist(exp(-select/256))

hist(select)


# para ntf2
detach(znbrm)
attach(ntf2)

#concentra-se entre 20 e 25; dois valores nulos
hist(TON)
hist(exp(-TON/128))

# concentra-se entre 80 e 100
hist(select)

#melhor distribuição
hist(exp(-select/256)


hist(Yield)
hist(exp(-Yield/256))

#bem distribuído
hist(TOF)
#
#para ionicp

#bem distribuído
hist(TON)

#super concentrado entre 80 e 100
hist(select)

#bem distribuído
hist(TOF)

# bem distribuído
hist(Yield)

#biplots
#fail
xyplot(select+Yield~Co2.sol,data=ionic2,auto.key=T)

tsel <-exp(-select/256)
tyie <-exp(-Yield/256)

#quase
xyplot(tsel+tyie~Co2.sol,data=ionic2,auto.key=T)

#nada
xyplot(tsel~Co2.sol,data=ionic2,auto.key=T)

#alguma coisa; testar esse nos outros 
xyplot(tyie~Co2.sol,data=ionic2,auto.key=T)



tsel <-exp(-select/128)

### selectivity co2.sol para ionic2 não serve




#parece decrescer para yield
xyplot(select+Yield~a.radii,data=ionic2,auto.key=T)

#+ou-
xyplot(tyie~a.radii,data=ionic2,auto.key=T)

#positive trend pra yield, logarithm trend pra select
xyplot(select+Yield~Dipty,data=ionic2,auto.key=T)

#fail
xyplot(tyie~Dipty,data=ionic2,auto.key=T)

#fail
xyplot(tsel+tyie~Tem,data=ionic2,auto.key=T)


xyplot(tsel+tyie~Tem,data=ionic2,auto.key=T)

#tem que ser investigado nos outrs sets
xyplot(tsel+tyie~Time,data=ionic2,auto.key=T)

#nada 
xyplot(tsel+tyie~Pr,data=ionic2,auto.key=T)
#nada
xyplot(tsel+tyie~mw,data=ionic2,auto.key=T)
#nada
xyplot(tsel+tyie~tg,data=ionic2,auto.key=T)
#interessante
xyplot(TON+TOF~tg,data=ionic2,auto.key=T)
xyplot(TON+TOF~Tem,data=ionic2,auto.key=T)
xyplot(TON+TOF~Co.sol,data=ionic2,auto.key=T)
xyplot(TON+TOF~Pr,dat=ionic2,auto.key=T)
#melhor
xyplot(TON+TOF~a.radii,data=ionic2,auto.key=T)
xyplot(TON+TOF~mw,data=ionic2,auto.key=T)
xyplot(TON+TOF~MP,data=ionic2,auto.key=T)

#ionic2 não serve

#fazer para znbrm
attach(znbrm)


xyplot(select+Yield~Co2.sol,data=znbrm,auto.key=T)

tsel <-exp(-select/256)
tyie <-exp(-Yield/256)

#quase
xyplot(tsel+tyie~Co2.sol,data=znbrm,auto.key=T)

#nada
xyplot(tsel~Co2.sol,data=znbrm,auto.key=T)

#alguma coisa 
xyplot(tyie~Co2.sol,data=znbrm,auto.key=T)



tsel <-exp(-select/128)

### selectivity co2.sol para ionic2 não serve




#parece decrescer para yield
xyplot(select+Yield~a.radii,data=znbrm,auto.key=T)

#inão
xyplot(tyie~a.radii,data=znbrm,auto.key=T)

#positive trend pra yield, logarithm trend pra select
xyplot(select+Yield~Dipty,data=znbrm,auto.key=T)

#parece decrescer para um set de dados 
xyplot(tyie~Dipty,data=ionic2,auto.key=T)


#fail
xyplot(tsel+tyie~Tem,data=znbrm,auto.key=T)

#tem que ser investigado nos outrs sets
xyplot(tsel+tyie~Time,data=znbrm,auto.key=T)

#nada 
xyplot(tsel+tyie~Pr,data=znbrm,auto.key=T)
#nada
xyplot(tsel+tyie~mw,data=znbrm,auto.key=T)
#nada
xyplot(tsel+tyie~tg,data=znbrm,auto.key=T)
#interessante
xyplot(TON+TOF~tg,data=znbrm,auto.key=T)
xyplot(TON+TOF~Tem,data=znbrm,auto.key=T)
#nada
xyplot(TON+TOF~Co2.sol,data=znbrm,auto.key=T)
xyplot(TON+TOF~Pr,data=znbrm,auto.key=T)
#melhor
xyplot(TON+TOF~a.radii,data=znbrm,auto.key=T)
#legal
xyplot(TON+TOF~mw,data=znbrm,auto.key=T)
#meio ruim
xyplot(TON+TOF~MP,data=znbrm,auto.key=T)

#para um set de dados
xyplot(TON~tg,data=znbrm,auto.key=T)
#parece interessante
xyplot(TOF~tg,data=znbrm,auto.key=T)

#show, ou não
xyplot(TON~tg,data=znbrm,auto.key=T)
#não
xyplot(TON~Tem,data=znbrm,auto.key=T)
#não
xyplot(TOF~Pr,data=znbrm,auto.key=T)
#não
xyplot(TOF~Tem,data=znbrm,auto.key=T)
#não
xyplot(TON~Pr,data=znbrm,auto.key=T)
#legal
xyplot(TOF~Time,data=znbrm,auto.key=T)
#não
xyplot(TON~Time,data=znbrm,auto.key=T)

xyplot(TON~exp((-Time*Tem*Pr)/5000),data=znbrm,auto.key=T,groups=Tem)

#para ntf2

#nada
xyplot(select+Yield~Co2.sol,data=ntf2,auto.key=T)

tsel <-exp(-select/256)
tyie <-exp(-Yield/256)

#nada
xyplot(tsel+tyie~Co2.sol,data=ntf2,auto.key=T)

#nada
xyplot(tsel~Co2.sol,data=ntf2,auto.key=T)

#nada
xyplot(tyie~Co2.sol,data=ntf2,auto.key=T)



tsel <-exp(-select/128)

### selectivity co2.sol para ionic2 não serve




#nada
xyplot(select+Yield~a.radii,data=ntf2,auto.key=T)

#nada
xyplot(tyie~a.radii,data=ntf2,auto.key=T)

#nada
xyplot(select+Yield~Dipty,data=ntf2,auto.key=T)

#nada
xyplot(tyie~Dipty,data=ionic2,auto.key=T)

#fail
xyplot(tsel+tyie~Tem,data=ntf2,auto.key=T)

#talvez
xyplot(tsel+tyie~Time,data=ntf2,auto.key=T)

#nada 
xyplot(tsel+tyie~Pr,data=ntf2,auto.key=T)
#nada
xyplot(tsel+tyie~mw,data=ntf2,auto.key=T)
#nada
xyplot(tsel+tyie~tg,data=ntf2,auto.key=T)
#nada
xyplot(TON+TOF~tg,data=ntf2,auto.key=T)
#not much
xyplot(TON+TOF~Tem,data=ntf2,auto.key=T)
#nada
xyplot(TON+TOF~Co.sol,data=ntf2,auto.key=T)
xyplot(TON+TOF~Pr,data=ntf2,auto.key=T)
#nada
xyplot(TON+TOF~a.radii,data=znbrm,auto.key=T)
#legal
xyplot(TON+TOF~mw,data=znbrm,auto.key=T)
#meio ruim
xyplot(TON+TOF~MP,data=znbrm,auto.key=T)

#para um set de dados
xyplot(TON~tg,data=znbrm,auto.key=T)
#parece interessante
xyplot(TOF~tg,data=znbrm,auto.key=T)

#show, ou não
xyplot(TON~tg,data=znbrm,auto.key=T)
#não
xyplot(TON~Tem,data=ntf2,auto.key=T)
#não
xyplot(TOF~Pr,data=ntf2,auto.key=T)
#não
xyplot(TOF~Tem,data=ntf2,auto.key=T)
#não
xyplot(TON~Pr,data=ntf2,auto.key=T)
#legal
xyplot(TOF~Time,data=ntf2,auto.key=T)
#sim
xyplot(TON~Time,data=ntf2,auto.key=T)
#bom plot
xyplot(TON~Time,data=ntf2,auto.key=T,groups=znbr)


#para ionicp


#good for yield
xyplot(select+Yield~Co2.sol,data=ionicp,auto.key=T)

tsel <-exp(-select/256)
tyie <-exp(-Yield/256)

#good for yield
xyplot(tsel+tyie~Co2.sol,data=ionicp,auto.key=T)

#mesmo que antes
xyplot(tsel~Co2.sol,data=ionicp,auto.key=T)

#mesmo que antes
xyplot(tyie~Co2.sol,data=ionicp,auto.key=T)



tsel <-exp(-select/128)

### selectivity co2.sol para ionic2 não serve




#nada
xyplot(select+Yield~a.radii,data=ionicp,auto.key=T)

#nada
xyplot(tyie~a.radii,data=ionicp,auto.key=T)

#nada
xyplot(select+Yield~Dipty,data=ionicp,auto.key=T)

#nada
xyplot(tyie~Dipty,data=ionicp,auto.key=T)

#fail
xyplot(tsel+tyie~Tem,data=ionicp,auto.key=T)

#talvez
xyplot(tsel+tyie~Time,data=ionicp,auto.key=T)

#nada 
xyplot(tsel+tyie~Pr,data=ionicp,auto.key=T)
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

#para um set de dados
xyplot(TON~tg,data=ionicp,auto.key=T)
#parece interessante
xyplot(TOF~tg,data=ionicp,auto.key=T)

# ou não
xyplot(TON~tg,data=ionicp,auto.key=T)
#não
xyplot(TON~Tem,data=ionicp,auto.key=T)
#não
xyplot(TOF~Pr,data=ionicp,auto.key=T)
#não
xyplot(TOF~Tem,data=ionic,auto.key=T)
#não
xyplot(TON~Pr,data=ionicp,auto.key=T)
#legal
xyplot(TOF~Time,data=ionicp,auto.key=T)
#sim
xyplot(TON~Time,data=ionicp,auto.key=T)
#bom plot
xyplot(TON~Time,data=ionicp,auto.key=T,groups=znbr)


#para ionic3 
ionic3 <-subset(ionic2,select2==no)

#nada 
xyplot(tsel+tyie~Pr,data=ionic3,auto.key=T)
#investigar; interessante
xyplot(tsel+tyie~mw,data=ionic3,auto.key=T)
#investigar; interessante 2 
xyplot(tsel+tyie~tg,data=ionic3,auto.key=T)
#investigar; interessante 3 
xyplot(TON+TOF~tg,data=ionic3,auto.key=T)
#not
xyplot(TON+TOF~Tem,data=ionic3,auto.key=T)
#+ ou -
xyplot(TON+TOF~Co2.sol,data=ionic3,auto.key=T)
#nada
xyplot(TON+TOF~Pr,data=ionic3,auto.key=T)
#interessante 5
xyplot(TON+TOF~a.radii,data=ionic3,auto.key=T)
#interessante 6
xyplot(TON+TOF~mw,data=ionic3,auto.key=T)
# interessante 7
xyplot(TON+TOF~MP,data=ionic3,auto.key=T)

