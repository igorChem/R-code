# Fazer quatro modelos para densidade
# Primeira para 303.15 e 20-30 bar 
# segundo pra 303.15 e 35-50 bar 
# terceiro para 323.15 e 35-50 bar 
#quarto para 323.15 e 20-30 bar 


# anexar a matriz de dados mãe
attach(sp32)
## matriz para a primeiro e segundo modelo
mpri <- subset(sp32,pres<35)
## matriz para o terceiro e quarto modelo 
mter <- subset(sp32, pres>31)

## primeiro modelo 

prmod <- lm(log(Densidade.303.15.)~log(pres),mpri)
summary(prmod)

##coeficientes do primeiro modelo 

#intercept 0.162774
#log(pres) 1.166080

## segundo modelo 

snmod <- lm(log(Densidade.303.15.)~log(pres),mter)
summary(snmod)

# coeficientes para o segundo modelo

#intercept -0.68055
#log(pres) 1.40458

##terceiro modelo 
trmod <-lm(log(Densidade.323.15.)~log(pres),mpri)
summary(trmod)

# coeficientes para o terceiro modelo 

#intercept 0.210407
# log (pres) 1.121489

## quarto modelo 

qrmod <-lm(log(Densidade.323.15.)~log(pres),mter)
summary(qrmod)

# coeficientes 

# intercept -0.260786
# log(pres) 1.255198


## modelos específicos para pressão inicial em temperaturas de 303 e 323

attach(Untitled.Document.1)

# modelo pressão inicial (20-25); tempertura 303.15

fthmod <-lm(log(Densidade.1.)~log(pres))
summary(fthmod)

# modelo pressão inicial (20-25); temp 323.15 

sthmod <-lm(log(Densidade.2.)~log(pres))
summary(sthmod)
