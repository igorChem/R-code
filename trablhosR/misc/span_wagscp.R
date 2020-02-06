# Span and wagner EOS 
# equations for density in function of temperatuere and pressure 

# 1. load the tables 
# 2. loading the first packges 
# -lattice
# -pls 

#visualizing data 

attach(spam_wag)
xyplot(densidade.kg.m...~Temperatura.K.+Press..o..Mpa.)
xyplot(densidade.kg.m...~(log(1/Temperatura.K.)))
xyplot(densidade.kg.m...~Press..o..Mpa.)
pca1 <-prcomp(spam_wag,scale. = T)
summary(pca1)
loadings(pca1)
plot(pca1)
biplot(pca1)
mod1 <-lm(densidade.kg.m...~Press..o..Mpa.)
summary(mod1)
tempe <-1/Temperatura.K.
mod2 <-lm(densidade.kg.m...~tempe)
summary(mod2)
mod3 <-lm(densidade.kg.m...~tempe+Press..o..Mpa.)
summary(mod3)
nd <- c(300,3)
predict(mod3)
mod4 <-mvr(densidade.kg.m...~Temperatura.K.+Press..o..Mpa.,2,method="oscorespls",validation="CV",data=spam_wag)
mod5 <-mvr(densidade.kg.m...~tempe+Press..o..Mpa.,2,method="oscorespls",validation="CV",data=spam_wag)
summary(mod4)
R2(mod4)
RMSEP(mod4)
summary(mod5)
spam_wag2 <- subset(spam_wag,Temperatura.K. > 300)
detach(spam_wag)
attach(spam_wag2)
tempe2 <-1/Temperatura.K.
mod6 <-mvr(densidade.kg.m...~tempe2*Press..o..Mpa.+Press..o..Mpa.,2,method="oscorespls",validation="CV",data=spam_wag2)
summary(mod6)
cp1 <- Press..o..Mpa.*Temperatura.K.
mod7 <-mvr(densidade.kg.m...~cp1+Press..o..Mpa.,2,method="oscorespls",validation="CV",data=spam_wag2)
attach(sp2)
predict(mod7,newdata = sp2,ncomp = 3)

xyplot(densidade.kg.m...~Temperatura.K.)
xyplot(densidade.kg.m...~exp(-Temperatura.K./8))
xyplot(log(densidade.kg.m...^0.2)~log(Press..o..Mpa.))
xyplot(densidade.kg.m...~1/(Press..o..Mpa.))
xyplot(densidade.kg.m...~Press..o..Mpa.)
# o melhor é modelo linear com pressão é linear. 
xyplot(densidade.kg.m...~Press..o..Mpa.*Temperatura.K.)
xyplot(densidade.kg.m...~-log(Press..o..Mpa.*Temperatura.K.))
xyplot(densidade.kg.m...~log(Press..o..Mpa.*Temperatura.K.^2))
xyplot(densidade.kg.m...~log((Press..o..Mpa.^-1.5)*Temperatura.K.))
xyplot(densidade.kg.m...~log((Press..o..Mpa.^-0.65)*Temperatura.K.))


xyplot(log(densidade.kg.m...)~log((Press..o..Mpa.^-0.58)*Temperatura.K.))

var1 <- log((Press..o..Mpa.^-0.62)*Temperatura.K.)
mod9 <-lm(log(densidade.kg.m...)~var1)
summary(mod9)
#criar variáveis transformadas para pls 
#log de densidade 
lden <-log(densidade.kg.m...)
#log da pressão 
lpres <-log(Press..o..Mpa.)

#modelo pls
mod10 <-mvr(lden~lpres+var1,2,method = "oscorespls", validation="CV")
summary(mod10)

#mod10 teve sucesso na montagem do modelo de predição da densidade em função da 
# pressão e temperatura. 
# coefficientes
# var1 = -1.4419051
# lpres = 0.1608158
# R2 = 0.99896
predplot(mod10, ncomp=2, type=c("o"))
residuals(mod10)
attach(spwag3)
xyplot(densidade.kg.m...~Press..o..Mpa.*Temperatura.K.)
xyplot(log(densidade.kg.m...)~log((Press..o..Mpa.^-0.62)*Temperatura.K.),type=c("o"))
mod11 <-mvr(lden~lpres+var1,2,method = "oscorespls", validation="CV")
mod11 <-mvr(lden~var1,method = "oscorespls", validation="CV")
spwag4 <-subset(spwag3, Press..o..Mpa.>1)
summary(mod11)
coefficients(mod11,intercept = T)

attach(spawag5)
xyplot(densidade~temp)
xyplot(log(densidade)~log((pres^-0.8)*(temp^1.05)),type="p")
spawag5 <-subset(spawag4,pres<50)

attach(spawag6)
de <-log(densidade)
v1 <-log((pres^-0.8)*(temp^1.05))
mod15 <-lm(de~v1)
summary(mod15)
residuals(mod15)
plot(residuals(mod15))
predplot(mod15)
d2 <-anova(mod15)
summary.aov(d2)
spawag6 <- subset(spawag6,pres<5.20)

mod17 <- mvr(de~v1+log1p(pres)+log1p(temp),method="oscorespls",validation="CV")
summary(mod17)
coefficients(mod17, intercept = T)
attach(spawag7)

attach(spw20)
spw202 <-subset(spw20,temp>290)
detach(spw20)
attach(spw202)
xyplot(densidade~temp)
xyplot(densidade~pres)
xyplot(log(densidade)~log((pres^-0.687)*(temp^1.05)),type="l")  
v3 <-log(densidade)
v4 <-log((pres^-0.687)*temp)
mod23 <-lm(v3~v4)
summary(mod23)
mod30 <-mvr(v3~v4+pres+temp,method = "oscorespls",validation = "CV")
summary(mod30)
R2(mod30)
predplot(mod30)
trc <- temp/304.128
prc <-pres/7.3777
rhor <-densidade/467.6

xyplot(log(rhor)~exp(-prc))
xyplot(log(rhor)~(prc^-0.819)*trc,type="p")
vfr <-(prc^-0.819)*trc
mod321 <-lm(log(rhor)~vfr)

mod31 <-mvr(pres+temp,method = "oscorespls",validation = "CV")

xyplot(log(pres)~log(densidade),type="o")
summary(mod321)
attach(sp32)
mod900 <-lm(log(densidade)~log(pres))
summary(mod900)
