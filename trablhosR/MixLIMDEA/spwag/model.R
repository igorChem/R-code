# modelos de interpolação da função de span-Wagner 


setwd("~/Dropbox/r/MixLIMDEA/spwag")
library(pls)

tab1 <-read.delim("spam_wag")

attach(tab1)

plot(Densidade~Temperatura)
plot(Densidade~Pressão)

fit1 <-lm(Densidade~Temperatura+Pressão) 
summary(fit1)
predplot(fit1)

fit2 <-lm(log(Densidade)~Temperatura+Pressão) 
summary(fit2)
predplot(fit2)

fit5 <-lm(log(Densidade)~log(Temperatura)+log(Pressão)) 
summary(fit5)
predplot(fit5)


fit5 <-lm(log(Densidade)~Temperatura:Pressão+log(Pressão)) 
summary(fit5)
predplot(fit5)

fit5 <-lm(log(Densidade)~Temperatura:Pressão+log(Pressão)+Pressão) 
summary(fit5)
predplot(fit5)

fit6 <-lm(log(Densidade)~Temperatura:Pressão+Pressão) 
summary(fit6)
predplot(fit6)
