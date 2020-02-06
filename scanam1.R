
library(ggplot2)


dat <-read.table("am1_scan_rd",head=T)
attach(dat)

tiff("hof.tiff",units="in",width=4,height=4,res=400)
plot(HOF~Molecule,data=dat,type="b",col="black",xlab="Step",ylab="Heat of Formation(kJ/mol)")
dev.off()

tiff("hardness.tiff",units="in",width=4,height=4,res=400)
plot(Hardness~Molecule,data=dat,type="b",col="green",xlab="Step",ylab="Hardness (eV)")
#plot(Electrophilicity~Molecule,data=dat,type="b",col="red",xlab="Step",ylab="Electrophilcity (eV)")
#plot(nMax~Molecule,data=dat,type="b",col="blue",xlab="Step",ylab="Max Electron Receible")
dev.off()


tiff("l_hard.tiff",units="in",width=5,height=5,res=600)
par(mfrow=c(2,2))
#plot(HOF~Molecule,data=dat,type="b",col="black",xlab="step",ylab="Heat of Formation(kJ/mol)")
plot(H.124.Hardness.~Molecule,data=dat,type="b",col="black",xlab="step",ylab="H42(NADH) Hardness")
plot(H.122.Hrdness.~Molecule,data=dat,type="b",col="blue",xlab="step",ylab="HG3(LYS65) Hardness")
plot(C.265.Hardness.~Molecule,data=dat,type="b",col="red",xlab="step",ylab="C3(DHK) Hardness")
plot(O.266.Hardness.~Molecule,data=dat,type="b",col="green",xlab="step",ylab="O3(DHK) Hardness")
dev.off()
tiff("l_easnas.tiff",units="in",width=5,height=5,res=600)
par(mfrow=c(2,2))
plot(C.265.NAS.~Molecule,data=dat,type="b",col="black",xlab="step",ylab="C3(DHK) NAS")
plot(H.122.NAS.~Molecule,data=dat,type="b",col="blue",xlab="step",ylab="HG3(LYS65)  NAS")
plot(H.124.EAS.~Molecule,data=dat,type="b",col="red",xlab="step",ylab="H42(NADH) EAS")
plot(O.266.EAS.~Molecule,data=dat,type="b",col="green",xlab="step",ylab="O3(DHK) EAS")
dev.off()

dat2 <-read.table("enolase_rd",head=T)
attach(dat2)

tiff("hof_en.tiff",units="in",width=4,height=4,res=400)
plot(HOF~Molecule,type="b",col="black",xlab="Step",ylab="Heat of Formation(kJ/mol)")
dev.off()

tiff("hardness_en.tiff",units="in",width=4,height=4,res=400)
plot(Hardness~Molecule,type="b",col="green",xlab="Step",ylab="Hardness (eV)")
dev.off()

tiff("electrophilicity_en.tiff",units="in",width=4,height=4,res=400)
plot(Electrophilicity~Molecule,type="b",col="red",xlab="Step",ylab="Electrophilcity (eV)")
dev.off()

tiff("nmax_en.tiff",units="in",width=4,height=4,res=400)
plot(nMax~Molecule,type="b",col="blue",xlab="Step",ylab="Max Electron Receible")
dev.off()

tiff("ecp_en.tiff",units="in",width=4,height=4,res=400)
plot(ECP~Molecule,type="b",col="blue",xlab="Step",ylab="Chemical Potential (eV)")
dev.off()

tiff("atomrd_en.tiff",units="in",width=6,height=8,res=600)
par(mfrow=c(2,2))
plot(OE3_Hardness~Molecule,type="b",col="black",xlab="Step",ylab="OE3(glu) Hardness")
plot(H1_Hardness~Molecule,type="b",col="blue",xlab="Step",ylab="H1(substrate) Hardness")
plot(OE3_EAS~Molecule,type="b",col="red",xlab="Step",ylab="OE3(glu) EAS")
plot(H1_NAS~Molecule,type="b",col="green",xlab="Step",ylab="H1(substrate) NAS")
dev.off()

tiff("pair_int.tiff",units="in",width=6,height=4,res=600)
par(mfrow=c(1,2))
plot(HPI~Molecule,type="b",col="black",xlab="Step",ylab="Hardness Pair Interaction")
plot(CT~Molecule,type="b",col="blue",xlab="Step",ylab="Charge Transfer")
dev.off()

