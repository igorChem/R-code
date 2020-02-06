# protein structure- energy calculation 

# contents
# 1. Load data and packages
# 2. Exploring the data 
# 3. Training models 


#=======================================================================

# 1. Load data and packages

library(pls)
library(ggfortify)
library(lattice)

setwd("~/igor/Dropbox/Dropbox/r/Protein")

setwd("~/Dropbox/r/Protein")

# loading the data set

dat1 <-read.table("data",header=T)

for (i in 4:12) {
	dat1[,i] <-as.numeric(dat1[,i])
} 

#=======================================================================

# 2. Exploring the data 

cor(dat1[,3:12])
pca1 <-prcomp(dat1[,3:12],scale=T)
summary(pca1)
biplot(pca1)
loadings(pca1)

attach(dat1)
detach(dat1)

plot(RMSD~EEL)
boxplot(RMSD~cut(EEL,10))
plot(RMSD~NB)
boxplot(RMSD~cut(NB,10))
plot(RMSD~DIHED)
boxplot(RMSD~cut(DIHED,10))
plot(RMSD~VDWAALS)
boxplot(RMSD~cut(VDWAALS,10))



pls1 <-mvr(RMSD~EEL+NB+DIHED+VDWAALS+EGB+ESURF+ANGLE+EELEC+BOND,9,data=dat1,scale=T,validation=c("CV"),method="oscorespls")
summary(pls1)

glm1 <-glm(RMSD~EEL+NB+DIHED+VDWAALS+EGB+ESURF+ANGLE+EELEC+BOND,family="poisson")
summary(glm1)
predplot(glm1)

exp(confint.default(glm1))

glm01 <-glm(RMSD~EEL*NB*DIHED*VDWAALS*EGB*ESURF*ANGLE*EELEC*BOND,family="poisson")
summary(glm01)
predplot(glm01)


glm2 <-glm(RMSD~EEL+DIHED+VDWAALS+ESURF+ANGLE+EELEC+EEL+
DIHED:VDWAALS+EEL:DIHED+EEL:EELEC,family="poisson")
summary(glm2)

exp(confint.default(glm2))
predplot(glm2)


dat5 <-subset(dat1,PDB=="1BDD")
dat5 <-subset(tab1,PDB=="1BDD")

X=dat5[,5:14]
y=dat5[,4]
fit1 <-krls(X=X,y=y)

newdata <-subset(tab1,PDB=="1CSK")

pred1 <-predict(fit1,newdata[,5:14])
pred1$fit
plot(pred1$fit~newdata[,4])


fit2 <-krls(X=tab1[,5:14],y=tab1[,4])


glm3 <-glm(RMSD~EEL+NB+DIHED+VDWAALS+EGB+ESURF+ANGLE+EELEC+BOND,family="poisson")
summary(glm3)
predplot(glm3)


glm4 <-glm(RMSD~EEL+DIHED+ESURF+ANGLE+EELEC,family="poisson")
summary(glm4)
predplot(glm4)

lm4 <-lm(RMSD~EEL+DIHED+ESURF+ANGLE+EELEC+VDWAALS+NB)
summary(lm4)
predplot(lm4)


#--------------------------------------------------------------------

tab1 <-read.table("logfile_Coef_hidro.energies",header=T)
summary(tab1)

tab2 <-tab1[,5:14]/tab1[,3]
summary(tab2)

attach(tab2)
plot(density(VDWAALS))
detach(tab2)

VDWAALS >=-0.243*10

tab3 <- data.frame(tab2,"RMSD"=tab1[,4])
attach(tab3)
detach(tab3)

pca2 <-prcomp(tab3,scale=T)
summary(pca2)
biplot(pca2)
loadings(pca2)

plot(RMSD~AB_energy)

lm5 <-lm(exp(-RMSD/4)~EEL*DIHED+ESURF*ANGLE+EELEC*VDWAALS+NB+
ESURF:DIHED+ESURF:EEL)
summary(lm5)
predplot(lm5)

tab4 <-tab1[,5:14]*tab1[,3]
summary(tab4)
tab4 <-data.frame(tab4,"RMSD"=tab1[,4])
attach(tab4)
detach(tab4)

lm6 <-lm(exp(-RMSD/4)~EEL*DIHED*ESURF*ANGLE*EELEC*VDWAALS*NB)
summary(lm6)
predplot(lm6)

#-----------------------------------------------------------------------

tab5 <-read.table("logfile_IT256bA.energies",header=T)
summary(tab5)
cor(tab5[,4:14])
attach(tab5)

lm7 <-lm((RMSD^0.3)~EEL+NB+DIHED+VDWAALS+EGB+ESURF)
summary(lm7)
predplot(lm7)
detach(tab5)

plot(RMSD~EEL)
plot(RMSD~log(EEL))
plot(RMSD~NB)
plot(RMSD~log(NB))
plot(RMSD~ESURF)
plot(RMSD~log(ESURF))
modEsurf <-ESURF^1/0.3
plot(RMSD^0.3~ESURF)
plot(RMSD~modEsurf)

lm8 <-lm(RMSD~modEsurf)
summary(lm8)
lm9 <-lm(RMSD^0.3~ESURF)
summary(lm9)
lm10 <-lm(RMSD~ESURF)
summary(lm10)

plot(RMSD~NB)
plot(RMSD~DIHED)
plot(RMSD^0.3~DIHED)

plot(RMSD~VDWAALS)
plot(RMSD^0.3~VDWAALS)

EGBmod <-EGB^1/0.3
plot(RMSD~EGBmod)

lm11 <-lm(RMSD~EGB)
summary(lm11)

lm12 <-lm(RMSD~EGBmod)
summary(lm12)

plot(RMSD~ANGLE)
plot(RMSD~BOND)
plot(RMSD~AB_energy)
plot(EELEC)

pca3 <-prcomp(tab5[4:14],scale=T)
summary(pca3)
par(mfrow=c(1,2))
biplot(pca3)
biplot(pca3,choices=c(1,3))
loadings(pca3)

#-----------------------------------------------------------------------
lm13 <-lm((RMSD^0.3)~EEL+NB+DIHED+VDWAALS+EGB+ESURF)
summary(lm13)
predplot(lm13)

lm14 <-lm(RMSD~EEL+NB+DIHED+VDWAALS+EGB+ESURF)
summary(lm14)
predplot(lm14)


#-----------------------------------------------------------------------

lm14 <-lm((RMSD^0.3)~EEL+NB+DIHED+VDWAALS+EGB+ESURF
+EEL:DIHED+ESURF:VDWAALS+ESURF:DIHED+VDWAALS:EGB)
summary(lm14)

library(KRLS)

df1 <-data.frame(EEL,NB,DIHED,VDWAALS,EGB,ESURF,BOND,ANGLE)

fit01.krls <-krls(X=df1,y=RMSD^0.3)
summary(fit01.krls)

plot(fit01.krls)


#----------------------------------------------------------------------

tab6 <-read.table("logfile.energies",header=T)
summary(tab6)

tab6 <-na.omit(tab6)
tab6 <-subset(tab6)
tab6 <-tab6[-2475,]
tab6 <-tab6[-2474,]
attach(tab6)

data1 <-data.frame(RMSD^0.3,EEL,NB,DIHED,VDWAALS,EGB,ESURF)

predicted <-predict(lm7,newdata=data1[,1:7])
plot(data1[,1]~predicted)

test1 <-fitted(lm10)

for (i in 1:length(test1)) {
	if (test1[i]>20) {print(test1[i])}
	}

pca10 <-prcomp(tab6[,3:12])
summary(pca10)
biplot(pca10)
cor(tab6[3:14)

autoplot(pca10,loadings=F,data=tab6,frame=T)


lm10 <-lm(RMSD^0.2~EEL+NB+DIHED+VDWAALS+EGB+ESURF)
summary(lm10)
tiff("plot1.tiff",res=220,units="in",width=6,height=6)
predplot(lm10,xlab="RMSD",ylab="RMSD Calculado")
dev.off()

tes <-(fitted(lm10))
tes2 <-RMSD^0.3
plot(tes~tes2)

tiff("plot1.tiff",res=220,units="in",width=6,height=6)
plot(RMSD^0.3~RMSD,ylab="RMSD transformado")
dev.off()

##======================================================================

tab7 <-read.table("logfile",header=T)
summary(tab7)

plot(tab7[,20])

#-----------------------------------------------------------------------
test3 <-tab7[,20]

for (i in 1:length(test3)) {
	if (test3[i] > 2000) {print(test3[i])}
	}
#-----------------------------------------------------------------------

attach(tab7)
names(tab7)

AA <-VDWAALS/SIZE


tab8 <-tab7[,5:20]/SIZE
tab8 <-data.frame(tab8,RMSD)
detach(tab7)
attach(tab8)



lm11 <-lm(RMSD^0.3~EEL+NB+DIHED+VDWAALS+EGB+ESURF+contacts0+AB_ENERGY)
summary(lm11)
predplot(lm11)


boxplot(residuals(lm11)|tab7[,2])


#-----------------------------------------------------------------------

tab200 <-read.table("newlog.txt",header=T)
summary(tab200)
attach(tab200)


lm12 <-lm(RMSD^0.3~EEL+NB+DIHED+VDWAALS+EGB+ESURF+AB_ENERGY+R_GYRATION+SIZE)
summary(lm12)
predplot(lm12)

pls1 <-mvr(RMSD^0.3~EEL+NB+DIHED+VDWAALS+EGB+ESURF+AB_ENERGY+R_GYRATION+SIZE,
	9,method="oscorespls")
summary(pls1)

