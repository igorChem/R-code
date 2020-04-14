library(pheatmap)
library(ggplot2)

globfdfd = read.table("globfdfd",header=T)
globfoafd = read.table("globfoafd",header=T)

foafoa = read.table("KAKA",header=T)
fdfd = read.table("FDFD",header=T)
foafd = read.table("FOAFD",header=T)

rowN <-c("AM1","RM1","PM3","PM6","PM7","AM1LMO","RM1LMO","PM3LMO","PM6LMO","PM7LMO")
rowN3 <-c("B3LYP(FOA)","AM1(FD)","RM1(FD)","PM3(FD)","PM6(FD)","PM7(FD)","AM1(FOA)","RM1(FOA)","PM3(FOA)","PM6(FOA)","PM7(FOA)","AM1LMO","RM1LMO","PM3LMO","PM6LMO","PM7LMO")
rowN0 <-c("AM1(FOA)","RM1(FOA)","PM3(FOA)","PM6(FOA)","PM7(FOA)","AM1LMO","RM1LMO","PM3LMO","PM6LMO","PM7LMO")
rowN2 <-c("AM1","RM1","PM3","PM6","PM7")

rowN4 <-c("JP2","JP3","0RB","1MX","RS8","19M")

datar = read.table("ricina",header=T)
attach(datar)

png("rta_results.png",width = 2000, height = 2000)
par(mfrow=c(2,2))

png("rta_EAS.png")
boxplot(EAS~type,col="blue",ylab="EAS")
dev.off()

png("rta_Dual.png")
boxplot(Dual~type,col="purple",ylab="Dual Fukui")
dev.off()

png("rta_RAS.png")
boxplot(EAS~type,col="gold",ylab="RAS")
dev.off()

png("rta_NAS.png")
boxplot(EAS~type,col="red",ylab="NAS")
dev.off()


boxplot(NAS~type,col="red",ylab="NAS")
boxplot(RAS~type,col="gold",ylab="RAS")
boxplot(Dual~type,col="purple",ylab="Dual Fukui")

ggplot(datar, aes(x=type, y=EAS)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
  
p<-ggplot(datar, aes(x=type, y=EAS, fill=type)) +
  geom_boxplot()
p 
  
rtamat <-data.matrix(datar[2:6])
rownames(rtamat) <- rowN4

foafoaMat <-data.matrix(foafoa[2:5])
fdfdMat   <-data.matrix(fdfd[2:5])
foafdMat  <-data.matrix(foafd[2:5])

gfdfdMat <-data.matrix(globfdfd[2:4])
gfoafdMat <-data.matrix(globfoafd[2:4])

rownames(foafoaMat) <-rowN
rownames(fdfdMat)   <-rowN0
rownames(foafdMat)  <-rowN3
rownames(gfdfdMat)  <-rowN2
rownames(gfoafdMat) <-rowN3

pheatmap(foafoaMat,color = colorRampPalette(c("white","cyan","blue"))(100),border_color=NA,cluster_cols=F,fontsize=15,main="\n",
filenam="l.l",width=12,height=16)

pheatmap(fdfdMat,color = colorRampPalette(c("white","lightgreen","green","black"))(200),border_color=NA,cluster_cols=F,fontsize=8,main="\n",
filenam="f.png",width=3.3,height=3.3)

pheatmap(foafdMat,color = colorRampPalette(c("white","navy"))(100),border_color=NA,cluster_cols=F,fontsize=8,main="\n",
filenam="foafd.pdf",width=3.3,height=3.3)

pheatmap(gfdfdMat,color = colorRampPalette(c("lightgrey","navy"))(100),border_color=NA,cluster_cols=F,fontsize=15,main="\n",
filenam="gfdfd.png",width=3.3,height=3.3)

pheatmap(gfoafdMat,color = colorRampPalette(c("lightgrey","navy"))(100),border_color=NA,cluster_cols=F,fontsize=15,main="\n",
filenam="gfoafd.png",width=3.3,height=3.3)

pheatmap(rtamat,color = colorRampPalette(c("lightgrey","navy"))(100),border_color=NA,cluster_cols=F,fontsize=8,main="\n",
filenam="rta.png",width=3.3,height=3.3)
