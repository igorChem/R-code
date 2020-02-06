# projeto misturas 

mixtab <-subset(mixILMDEA,mix=="yes")
#tabela para fazer modelos de mistura, com resposta =fração molar de CO2
mixtabt1 <-subset(mixtab,Temperatura=="303")
mixtabt2 <-subset(mixtab,Temperatura=="323")

attach(mixtabt1)
tab1 <-data.frame(x1,x2,x3,Fra....o)
detach(mixtabt1)
attach(mixtab2)
tab1 <-data.frame(x1,x2,x3,Fra....o)
xyplot(Fra....o~x1+x2+x3|Temperatura,data=mixtab,auto.key = T,cex=1.05,type="smooth")
#modmix1 ficou interessante
modmix1 <-MixModel(mixtab,"Fra....o",mixcomps = c("x1","x2","x3"),model=5,procvars = c("Temperatura"))
modmix12 <-MixModel(mixtab,"Fra....o",mixcomps = c("x1","x2","x3"),model=4)
modmix2 <-MixModel(mixtab,"pre..o",mixcomps = c("x1","x2","x3"),model=1)

ModelPlot(modmix1,dimensions = list(x1="x1",x2="x2",x3="x3"),
          slice = list(process.vars=c(z=1)), lims=c(.403,.704,.166,.467,.130,.431), main="z=1",
          constraints=TRUE,contour=TRUE,cuts=5,fill=FALSE,
          axislabs=c("Fraction Course","Fraction Fine","Fraction Binder"),
          cornerlabs = c("Course", "Fine", "Binder"),pseudo=TRUE)

#fazer com temperatura separadp 

