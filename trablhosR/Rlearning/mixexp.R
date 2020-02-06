# Learning Packges 
# mixexp 
#The package : 
#The mixexp package provides functions for creating mixture experiment designs in an uncon-
#strained simplex or constrained mixture space. Functions are also provided for making ternary
#contour plots, pictures of constrained regions, design points, and mixture effect plots.



# 1- Modeleff function
]
#Examples : 1 
#Example p. 63-65 Cornell (control of Mites)
# Four Component Mixture
mite<-SCD(4)
yavg<-c(1.8,25.4,28.6,38.5,4.9,3.1,28.7,3.4,37.4,10.7,22.0,2.6,2.4,
        11.1,0.8)
mite<-cbind(mite,yavg)
# cbind function was used to join the responses and the mixture matrix
miteSC<-MixModel(mite,"yavg",mixcomps=c("x1","x2","x3","x4"),model=4)
ModelEff(nfac=4,mod=4,nproc=0,dir=2,ufunc=miteSC,lc=c(0,0,0,0),uc=c(1,1,1,1))
plot(miteSC)
# Essa função serve para plotar os efeitos de mistura para vários componentes e combinações de efeitos.

# 2- function. DesignPoints

#Examples 
dat<-SCD(3)
DesignPoints(des=dat)
x1<-c(1,0,0,.5,.5, 0,.33333)
x2<-c(0,1,0,.5,0,.5,.33333)
x3<-c(0,0,1,0,.5,.5,.33333)
DesignPoints(x=x3,y=x2,z=x1)
dat<-data.frame(x1,x2,x3)
DesignPoints(des=dat)

#esa função desenha gráficos ternários de mistura
#da para mudar os axis labels 

# 3- function mixmodel 
#MixModel
#Fit mixture and mixture process variable models.

# Example
# example from Myers and Montgomery(2002), special cubic model
library(mixexp)
etch<-SCD(3)
etch<-Fillv(3,etch)
etch<-rbind(etch[1:7, ],etch[1:3, ],etch[7, ], etch[etch$x1==2/3, ],
            etch[etch$x2==2/3, ],etch[etch$x3==2/3, ])
erate<-c(540,330,295,610,425,330,800,560,350,260,850,710,640,460)
etch<-cbind(etch,erate)
mixvars<-c("x1","x2","x3")
response<-c("erate")
MixModel(etch,response,mixvars,4)

# essa fução faz modelos de mixtura para diversos tipos de modelos matématicos de regressão prévios
# e calcula direto o R-square e faz anova

# 4 function- ModelPlot
# This function makes contour plots of a user-supplied model in the sim-
#  plex mixture space.



#constrained plot mixture tem que usar lims e constraints argument
#vector of lower and upper constraints for ternary plot components (TopLower,
#                                                                   TopUpper, LeftLower, LeftUpper, RightLower, RightUpper).
#if TRUE constraints found in lims will be added to the graph

# Cornell s (2002) Yarn elongation
x1<-c(1,1,.5,.5,.5,0,0,0,0,0,0,0,.5,.5,.5)
x2<-c(0,0,.5,.5,.5,1,1,.5,.5,.5,0,0,0,0,0)
x3<-c(0,0,0,0,0,0,0,.5,.5,.5,1,1,.5,.5,.5)
y<-c(11,12.4,15,14.8,16.1,8.8,10,10,9.7,11.8,16.8,16,17.7,16.4,16.6)
elong<-data.frame(x1,x2,x3,y)
testQ<-lm(y~-1+x1+x2+x3+x1:x2+x1:x3+x2:x3,data=elong)
ModelPlot(model = testQ,dimensions = list(x1="x1",x2="x2",x3="x3"),
          main="Thread Elongation",constraints=FALSE,contour=TRUE,
          at=c(12, 13, 14, 15, 16, 17),fill=T,
          axislabs=c("X1", "X2", "X3"),
          cornerlabs = c("X1", "X2", "X3"),pseudo=FALSE)


#example of misture ternary plots for constraints mixture plots 
#### Kowalski Cornell and Vining Simplified model on data from Gallant et. al. (2008)
data(Burn)
testBNM<-MixModel(Burn,"y",mixcomps=c("Course","Fine","Binder"),model=6,procvars=c("z"))
ModelPlot(testBNM,dimensions = list(x1="Course",x2="Fine",x3="Binder"),
          slice = list(process.vars=c(z=1)), lims=c(.403,.704,.166,.467,.130,.431), main="z=1",
          constraints=TRUE,contour=TRUE,cuts=5,fill=TRUE,
          axislabs=c("Fraction Course","Fraction Fine","Fraction Binder"),
          cornerlabs = c("Course", "Fine", "Binder"),pseudo=TRUE)

# 5- function SCD, This function creates simplex lattice designs in unconstrained mixture experiment space.
# example des<-SCD(5)

# 6- function xvert, This function creates an extreme vertices design in a constrained mix-
# ture space
# Example 
Xvert(nfac=3,uc=c(1,0.4,0.2),lc=c(0.4,0,0),ndm=1)

# essa função ainda faz o plot da mixture

