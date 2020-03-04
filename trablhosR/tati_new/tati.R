require(ggpubr)

dat <-read.table("tati_data",header=T)
attach(dat)


#----------------------------------------------------------------------

p1 <- ggplot(dat,aes(x=Membrane,y=Degradation,color=Membrane))+
	geom_boxplot(notch=T) +
	labs(title="",
       x="Membrane", y ="Degradation")
       
png("box_membranexDegrad.png",width = 4, height = 4.5, units = 'in', res = 1000)
      
p1 

dev.off()


#----------------------------------------------------------------------
p2 <- ggplot(dat,aes(x=Membrane,y=Mineralization,color=Membrane))+
	geom_boxplot(notch=T) +
	labs(title="",
       x="Membrane", y ="Mineralization")

png("box_membranexMine.png",width = 4, height = 4.5, units = 'in', res = 1000)

p2 
	
dev.off()

#----------------------------------------------------------------------

png("timexdegrad.png",width = 4, height = 4.5, units = 'in', res = 1000)

p4 <-qplot(t, Degradation, data = dat, color = Membrane,
      geom=c("point", "smooth"))        +
	labs(title="",
    x="Time(s)", y ="Degradation")
p4
dev.off()
 
#----------------------------------------------------------------------

png("timexMine.png",width = 4, height = 4.5, units = 'in', res = 1000)
   
p5 <-qplot(t, Mineralization, data = dat, color = Membrane,
      geom=c("point", "smooth"))        +
	labs(title="",
    x="Time(s)", y ="Mineralization")
    p5 
dev.off()
    
#--------------------------------------------------------------------
png("currentxDegrad.png",width = 4, height = 4.5, units = 'in', res = 1000)

p4 <-qplot(i, Degradation, data = dat, color = Membrane,
      geom=c("point", "smooth"))        +
	labs(title="",
    x="Current(mA.cm-2)", y ="Degradation")
p4
dev.off()
 
	  #--------------------------------------------------------------------
png("currentxMine.png",width = 4, height = 4.5, units = 'in', res = 1000)
   
p5 <-qplot(i, Mineralization, data = dat, color = Membrane,
      geom=c("point", "smooth"))        +
	labs(title="",
    x="Current(mA.cm-2)", y ="Mineralization")
    p5 
dev.off()
#--------------------------------------------------------------------

png("naso4xDegrad.png",width = 4, height = 4.5, units = 'in', res = 1000)

p4 <-qplot(X.Na2SO4., Degradation, data = dat, color = Membrane,
      geom=c("point", "smooth"))        +
	labs(title="",
    x="[NaSO4]", y ="Degradation")
p4
dev.off()
 
	  #--------------------------------------------------------------------
png("naso4xMine.png",width = 4, height = 4.5, units = 'in', res = 1000)
   
p5 <-qplot(X.Na2SO4., Mineralization, data = dat, color = Membrane,
      geom=c("point", "smooth"))        +
	labs(title="",
    x="[NaSO4]", y ="Mineralization")
    p5 
dev.off()
