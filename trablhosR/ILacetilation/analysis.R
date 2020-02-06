## 

# exploração de correlações entre Descritores de reatividade do sistema cataliticoo 

#=======================================================================
# 0. load packges
#=======================================================================

library(ggfortify)
library(pls)


setwd("~/Dropbox/r/ILacetilation")

tab1 <-read.delim('data')


cor(tab1[3:14)


attach(tab1)
boxplot(Conversão...~Acylating)

plot(Conversão...~Hardness,col=Acylating) # vou ter que seprar por tipo de agente acilante
plot(Conversão...~HardnessSimilarity.AA.,col=Acylating) # mesmo que acima
plot(Conversão...~HardnessSimilarity.Glycerol.,col=Acylating)

plot(Conversão...~Electrophilicity,col=Acylating)
plot(Conversão...~deltaN.Glycerol.,col=Acylating) # separar AA 
plot(Conversão...~deltaN.AA.,col=Acylating)



var1 <- Hardness*Electrophilicity
plot(Conversão...~var1)



detach(tab1)

tab2 <- tab1[-14,]

attach(tab2)
plot(Conversão...~LocalHardnessE,col=Acylating)
plot(Conversão...~FukuiN,col=Acylating)
plot(Conversão...~LocalHardnessN,col=Acylating) # boa, separar
plot(Conversão...~FukuiE,col=Acylating)


#-----------------------------------------------------------------

tab3 <- subset(tab1,Acylating=='AcOH')

detach(tab2)
attach(tab3)


#=======================================================================


a1 <- first <-ggplot(tab3, aes(x=Hardness, y=Conversão...)) +
		geom_point(shape=19,cex=2.5,col="blue") +
		geom_smooth(method=lm) +  
        labs(x="Hardness (eV)",cex=1.5)+
        labs(y="conversion (%)",cex=1.5)
        
    a1
    
b1 <- first <-ggplot(tab3, aes(x=HardnessSimilarity.AA., y=Conversão...)) +
		geom_point(shape=19,cex=2.5,col="black") +
		geom_smooth(method=lm) +  
        labs(x=" Hardness similarity \n with AcOH (eV)",cex=1.5)+
        labs(y="conversion (%)",cex=1.5)
        
    b1
    
c1 <- first <-ggplot(tab3, aes(x=deltaN.Glycerol., y=Conversão...)) +
		geom_point(shape=19,cex=2.5,col="green") +
		geom_smooth(method=lm) +  
        labs(x="Electron flow to glycerol (eV)",cex=1.5)+
        labs(y="conversion (%)",cex=1.5)
        
    c1
    
tiff("descriptorsXconversion.tiff",res=220,units="in",width=8,height=6)    
multiplot(a1,b1,c1,cols=3)    
dev.off()

lm1 <- lm(Conversão...~Hardness)
summary(lm1)

lm2 <- lm(Conversão...~HardnessSimilarity.AA.)
summary(lm2)

lm3 <- lm(Conversão...~deltaN.Glycerol.)
summary(lm3)

#=======================================================================
 
plot(Conversão...~Hardness)
plot(Conversão...~HardnessSimilarity.AA.)
plot(Conversão...~deltaN.Glycerol.)

#=======================================================================

plot(Conversão...~LocalHardnessN) # boa

par(mfrow=c(2,2))
plot(Conversão.2M.~LocalHardnessN)
plot(Conversão.1.M.~LocalHardnessN)
plot(Conversão.1.2D.~LocalHardnessN)
plot(Conversão.1.3D.~LocalHardnessN)


#=======================================================================


first <-ggplot(tab3, aes(x=LocalHardnessN, y=Conversão...)) +
		geom_point(shape=19,cex=2.5,col="red") +
		geom_smooth(method=lm) +  
        labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5)+
        labs(y="conversion (%)",cex=1.5)
        
    first

##----------------------------------------------------------------------
a <-ggplot(tab3, aes(x=LocalHardnessN, y=Conversão.1.M.)) +
  geom_point(shape=19,cex=2.5,col="black") +
  geom_smooth(method=lm) +
  labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5)+
  labs(y="conversion 1M (%)",cex=1.5)
  
  a
     
##----------------------------------------------------------------------
     
b <-ggplot(tab3, aes(x=LocalHardnessN, y=Conversão.2M.)) +
  geom_point(shape=19,cex=2.5,col="blue") +
  geom_smooth(method=lm) +
  labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5)+
  labs(y="conversion 2M (%)",cex=1.5)
  
  b
         
##---------------------------------------------------------------------- 
     
c <-ggplot(tab3, aes(x=LocalHardnessN, y=Conversão.1.2D.)) +
  geom_point(shape=19,cex=2.5,col="green") +
  geom_smooth(method=lm) +   
  labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5) +
  labs(y="conversion 1.2D (%)",cex=1.5)
     
  c
##----------------------------------------------------------------------
         
d <-ggplot(tab3, aes(x=LocalHardnessN, y=Conversão.1.3D.)) +
  geom_point(shape=19,cex=2.5,col="yellow") +
  geom_smooth(method=lm) +
  labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5)+
  labs(y="conversion 1.3D (%)",cex=1.5)
  
  d
           
##----------------------------------------------------------------------

e <-ggplot(tab3, aes(x=LocalHardnessN, y=Conversão.T.)) +
  geom_point(shape=19,cex=2.5,col="orange") +
  geom_smooth(method=lm) +
  labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5)+
  labs(y="conversion T (%)",cex=1.5)    
  
  e     
#=======================================================================           
           
           
d1 <-  ggplot(tab3, aes(x=LocalHardnessN, y=Conversão.1.3D.)) +
  geom_point(shape=19,cex=2.5,col="yellow") +
  geom_smooth(method=lm) + 
  xlim(min(Conversão.1.3D.),2.3) +
  labs(x="Local Hardness \n nucleophilicity (eV)",cex=1.5)+
  labs(y="conversion 1,3D (%)",cex=1.5)    
  
  d1       
           
tiff("localhardnessXconversion.tiff",res=220,units="in",width=6,height=6)
          
multiplot(first,a,b,c,d1,e,cols=3)  

dev.off()   
