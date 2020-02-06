require(ggpubr)

#======================================================================
# tim reaction analysis
tim_dat <-read.table("tim_rd",header=T)
attach(tim_dat)

#global variables plots
#------------------------------------------------------------------
#Heat of formation for the frames for each Hamiltonian
p1 <- ggplot(tim_dat,aes(x=n,y=HOF,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="frame", y = expression(paste(Delta,"H(kcal/mol)")))
p1 

#------------------------------------------------------------------
#Hardness for the frames for each Hamiltonian
p2 <- ggplot(tim_dat,aes(x=n,y=Hardness,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness(eV)")       
p2

#------------------------------------------------------------------
#Electronic energy change for each Hamiltonian
p3 <- ggplot(tim_dat,aes(x=n,y=Energy,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = expression(paste(Delta,"E(eV)")))       
p3


#------------------------------------------------------------------
p4 <- ggplot(tim_dat,aes(x=n,y=ECP,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y ="ECP(eV)")      
p4
#------------------------------------------------------------------
#correlation between Hardness and Heat of Formations 
sp <- ggscatter(tim_dat,x="Energy",y="Hardness",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                size = 0.15,
                palette = c("blue", "orange","purple","green", "red"),
                palette = "jco"         , 
                shape = "Hamiltonian"                 
                )+
  stat_cor(aes(color = Hamiltonian), label.x = 0.6) 
sp
#------------------------------------------------------------------
fig1 <-ggarrange(p1,p2,p3,p4, 
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2)

png("glob_tim2",width = 6, height = 5, units = 'in', res = 1000)
fig1
dev.off()
#
tim_dat$HPI_A <- 100*tim_dat$HPI_A
tim_dat$HPI_C <- 100*tim_dat$HPI_C
#------------------------------------------------------------------
p5 <- ggplot(tim_dat,aes(x=n,y=HPI_A,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p5
#------------------------------------------------------------------
p6 <- ggplot(tim_dat,aes(x=n,y=HPI_B,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p6
#------------------------------------------------------------------
p7 <- ggplot(tim_dat,aes(x=n,y=HPI_C,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p7
#------------------------------------------------------------------
tim_dat <-read.table("tim_rd",header=T)
p8 <- ggplot(tim_dat,aes(x=n,y=HPI_D,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p8

png("hpiD.png",width = 4, height = 4, units = 'in', res = 400)
p8
dev.off()
#------------------------------------------------------------------
fig2 <-ggarrange(p5,p6,p7,p8,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_tim2.png",width = 6, height = 5, units = 'in', res = 400)
fig2
dev.off()
#------------------------------------------------------------------
sp2_A <- ggscatter(tim_dat,x="HOF",y="HPI_A",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                palette = c("blue", "orange","purple","green", "red"),
                shape = "Hamiltonian",
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction",               
                )+
  stat_cor(aes(color = Hamiltonian), label.x =0.2) 
sp2_A
#------------------------------------------------------------------
sp2_B <- ggscatter(tim_dat,x="HOF",y="HPI_B",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,     
                palette = c("blue", "orange","purple","green", "red"),         
                color = "Hamiltonian"   , 
                shape = "Hamiltonian"  ,
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction",                
                )+
  stat_cor(aes(color = Hamiltonian), label.x = 0.6) 
sp2_B
#------------------------------------------------------------------
sp2_C <- ggscatter(tim_dat,x="HOF",y="HPI_C",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                palette = c("blue", "orange","purple","green", "red"), 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction", 
                shape = "Hamiltonian"                 
                )+
  stat_cor(aes(color = Hamiltonian), label.x = 0.3) 
sp2_C
#------------------------------------------------------------------
sp2_d <- ggscatter(tim_dat,x="HOF",y="HPI_D",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                palette = c("blue", "orange","purple","green", "red"), 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction", 
                shape = "Hamiltonian"                 
                )+
  stat_cor(aes(color = Hamiltonian), label.x = 0.6) 
sp2_d
#------------------------------------------------------------------
png("hpi_cor_tim2A.png",width = 6, height = 6, units = 'in', res = 1000)
sp2_A
dev.off()
png("hpi_cor_tim2B.png",width = 6, height = 6, units = 'in', res = 1000)
sp2_B
dev.off()
png("hpi_cor_tim2C.png",width = 6, height = 6, units = 'in', res = 1000)
sp2_C
dev.off()
png("hpi_cor_tim2D.png",width = 6, height = 6, units = 'in', res = 1000)
sp2_d
dev.off()
#------------------------------------------------------------------
p9 <- ggplot(tim_dat,aes(x=n,y=SPI,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Softness Pair Interaction")       
p9
#------------------------------------------------------------------
p10 <- ggplot(tim_dat,aes(x=n,y=CT,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Charge Transfer")       
p10
#------------------------------------------------------------------
fig3 <-ggarrange(p9,p10,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

png("soft_chg_tim2.png",width = 4, height = 6, units = 'in', res = 1000)
fig3
dev.off()
#------------------------------------------------------------------
p11 <- ggplot(tim_dat,aes(x=n,y=eas_a1,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "EAS OE2(GLU164)")       
p11
#------------------------------------------------------------------
p12 <- ggplot(tim_dat,aes(x=n,y=nas_a2,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "NAS H(DHAP)")       
p12
#------------------------------------------------------------------
p13 <- ggplot(tim_dat,aes(x=n,y=hardness_a1,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness OE2(GLU164)")       
p13
#------------------------------------------------------------------
p14 <- ggplot(tim_dat,aes(x=n,y=hardness_a2,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness H(DHAP)")       
p14

#------------------------------------------------------------------
fig4 <-ggarrange(p11,p12,p13,p14,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)
#------------------------------------------------------------------
png("atom_rd_tim2.png",width = 7, height = 6, units = 'in', res = 1000)
fig4
dev.off()
#=======================================================================
#Deahalogenase graphs 
deh_dat <-read.table("dehalo_rd",header=T)
attach(deh_dat)

#------------------------------------------------------------------
#Heat of formation for the frames for each Hamiltonian
p17 <- ggplot(deh_dat,aes(x=n,y=HOF,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = expression(paste(Delta,"H(kcal/mol)")))
p17 


#------------------------------------------------------------------
#Hardness for the frames for each Hamiltonian
p18 <- ggplot(deh_dat,aes(x=n,y=Hardness,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness(eV)")       
p18

png("dehalo_glob_energy.png",width = 5, height = 5, units = 'in', res = 400)
p17
dev.off()
#------------------------------------------------------------------
#Electronic energy change for each Hamiltonian
p19 <- ggplot(deh_dat,aes(x=n,y=Energy,color=Hamiltonian,shape=Hamiltonian))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = expression(paste(Delta,"E(eV)")))       
p19
#------------------------------------------------------------------
p20 <- ggplot(deh_dat,aes(x=n,y=ECP,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Chemical Potential(eV)")       
p20
#------------------------------------------------------------------
fig5 <-ggarrange(p17,p18,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
          
png("dehalo_glob.png",width = 7, height = 5, units = 'in', res = 400)
fig5
dev.off()
#------------------------------------------------------------------
p21 <- ggplot(deh_dat,aes(x=n,y=HPI_A,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p21
#------------------------------------------------------------------
p22 <- ggplot(deh_dat,aes(x=n,y=HPI_B,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p22
#------------------------------------------------------------------
p23 <- ggplot(deh_dat,aes(x=n,y=HPI_C,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p23
#------------------------------------------------------------------
p24 <- ggplot(deh_dat,aes(x=n,y=HPI_D,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Pair Interaction")       
p24
#------------------------------------------------------------------
fig6 <-ggarrange(p21,p22,p23,p24,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_dehalo.png",width = 7, height = 5, units = 'in', res = 400)
fig6
dev.off()
#---------------------------------------------------------------------
p25 <- ggplot(deh_dat,aes(x=n,y=eas_a1,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "EAS Cl")       
p25
#------------------------------------------------------------------
p26 <- ggplot(deh_dat,aes(x=n,y=nas_a2,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "NAS C2(DCE)")       
p26
#------------------------------------------------------------------
p27 <- ggplot(deh_dat,aes(x=n,y=hardness_a1,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness Cl")       
p27
#------------------------------------------------------------------
p28 <- ggplot(deh_dat,aes(x=n,y=hardness_a2,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Hardness C2(DCE)")       
p28
#------------------------------------------------------------------
p29 <- ggplot(deh_dat,aes(x=n,y=chg_a1,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Partial Charge Cl")       
p29
#------------------------------------------------------------------
p30 <- ggplot(deh_dat,aes(x=n,y=chg_a2,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Partial Charge C2(DCE)")       
p30
#------------------------------------------------------------------
fig7 <-ggarrange(p25,p26,p27,p28,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)
#------------------------------------------------------------------
png("atom_rd_dehalo.png",width = 6, height = 5, units = 'in', res = 1400)
fig7
dev.off()
#------------------------------------------------------------------
p31 <- ggplot(deh_dat,aes(x=n,y=SPI,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Softness Pair Interaction")       
p31
#------------------------------------------------------------------
p32 <- ggplot(deh_dat,aes(x=n,y=CT,color=method,shape=method))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame", y = "Charge Transfer")       
p32
#------------------------------------------------------------------
fig8 <-ggarrange(p31,p32,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

png("soft_chg_dehalo.png",width = 4, height = 6, units = 'in', res = 400)
fig8
dev.off()
#--------------------------------------------------------------------
deh_dat$HPI_A <- 100*deh_dat$HPI_A
deh_dat$HPI_C <- 100*deh_dat$HPI_C
deh_dat$hardness_a1 <- 100*deh_dat$hardness_a1
deh_dat$hardness_a2 <- 100*deh_dat$hardness_a2
#------------------------------------------------------------------
sp2_1 <- ggscatter(deh_dat,y="HOF",x="SPI",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                pallete = c("blue", "orange","purple","green", "red"),
                shape = "Hamiltonian",
                ylab = expression(paste(Delta,"H(kcal/mol)")),
                xlab = "Total Hardenss Pair Interaction",               
                )+
  stat_cor(aes(color = Hamiltonian), label.x =0.0003) 
sp2_1
#------------------------------------------------------------------
sp2_2 <- ggscatter(deh_dat,y="HOF",x="CT",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                pallete = c("blue", "orange","purple","green", "red"),
                shape = "Hamiltonian",
                ylab = expression(paste(Delta,"H(kcal/mol)")),
                xlab = "Total Hardenss Pair Interaction",               
                )+
  stat_cor(aes(color = Hamiltonian), label.x =0.0003) 
sp2_2

#------------------------------------------------------------------
sp2_A <- ggscatter(deh_dat,x="HOF",y="HPI_A",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,                   
                palette = c("blue", "orange","purple","green", "red"),         
                color = "Hamiltonian"   , 
                shape = "Hamiltonian"  ,
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction",                
                )+
  stat_cor(aes(color = Hamiltonian), label.x = -70,label.y.npc="bottom") 
sp2_A

sp2_B <- ggscatter(deh_dat,x="HOF",y="HPI_B",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,     
                palette = c("blue", "orange","purple","green", "red"),         
                color = "Hamiltonian"   , 
                shape = "Hamiltonian"  ,
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction",                
                )+
  stat_cor(aes(color = Hamiltonian), label.x = -70,label.y.npc ="top") 
sp2_B
#------------------------------------------------------------------
sp2_C <- ggscatter(deh_dat,x="HOF",y="HPI_C",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                palette = c("blue", "orange","purple","green", "red"), 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction", 
                shape = "Hamiltonian"                 
                )+
  stat_cor(aes(color = Hamiltonian), label.x = -70) 
sp2_C
#------------------------------------------------------------------
sp2_d <- ggscatter(deh_dat,x="HOF",y="HPI_D",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,               
                color = "Hamiltonian"   , 
                palette = c("blue", "orange","purple","green", "red"), 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction", 
                shape = "Hamiltonian"                 
                )+
  stat_cor(aes(color = Hamiltonian), label.x = -70) 
sp2_d

png("hpideh_Acor.png",width = 5, height = 6, units = 'in', res = 1000)
sp2_A
dev.off()
png("hpideh_Bcor.png",width = 5, height = 6, units = 'in', res = 1000)
sp2_B
dev.off()
png("hpideh_Ccor.png",width = 5, height = 6, units = 'in', res = 1000)
sp2_C
dev.off()
png("hpideh_Dcor.png",width = 5, height = 6, units = 'in', res = 1000)
sp2_d
dev.off()


#=======================================================================
# Adenosine kinase 2d reactivity descriptors plot using mozyme
ak_rd <-read.table("ak_rd",header=T)
ak_rd_pm3 <-subset(ak_rd,method=="PM3")
ak_rd_pm6 <-subset(ak_rd,method=="PM6")
ak_rd_am1 <-subset(ak_rd,method=="AM1")

#-----------
#pm3 ak
require(lattice)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

p2d_1 <- ggplot(ak_rd_pm, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Heat of \nFormation \n(kcal/mol)"))
p2d_1

p2d_2 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = ECP)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ECP)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(70))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Chemical \n Potential\n(eV)"))
p2d_2

p2d_3 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(70))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Hardness(eV)"))
p2d_3

p2d_4 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = Electrophilicity)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =Electrophilicity )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Softness(eV)"))
p2d_4


p2d_5 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
         xlab("P1(ATP)--O1(ADN)") +
         ylab("H1(ADN)--OD2(ASP)") +
         #geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total"))
p2d_5

p2d_5 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         xlab("P1(ATP)--O1(ADN)") +
         ylab("H1(ADN)--OD2(ASP)") +
         #geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total"))
p2d_5


p2d_5 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D)) +
         stat_contour(bins = 10,color="black") +
         xlab("P1(ATP)--O1(ADN)") +
         ylab("H1(ADN)--OD2(ASP)") +
         #geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total"))
p2d_5


p2d_6 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = eas_a3)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =eas_a3 )) +
         stat_contour(bins = 10,color="black")+
         xlab("P1(ATP)--O1(ADN)") +
         ylab("H1(ADN)--OD2(ASP)") +
         #geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI P1(ATP) O1(ADN)"))
p2d_6

p2d_7 <- ggplot(ak_rd, aes(x = m, y = n, z = HPI2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12 )) +
         stat_contour(bins = 10,color="black") +
         xlab("P1(ATP)--O1(ADN)") +
         ylab("H1(ADN)--OD2(ASP)") +
         #geom_text_contour()+
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI H1(ADN)--OD2(ASP)"))
p2d_7



fig9 <-ggarrange(p2d_1,p2d_2,p2d_3,p2d_4,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("glob_2d_ak_pm7.png",width = 8, height = 6, units = 'in', res = 600)
fig9
dev.off()
#------
png("HOF1_2d_ak_pm7.png",width = 8, height = 6, units = 'in', res = 600)
xyplot(HOF~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="Heat of Formation(kcal/mol)")
dev.off()

png("HOF2_2d_ak_pm7.png",width = 9, height = 6, units = 'in', res = 600)
xyplot(HOF~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP18)",ylab="Hardness Pair Interaction")
dev.off()

png("HPI1_2d_ak_pm7.png",width = 8, height = 6, units = 'in', res = 600)
xyplot(HPI1~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="Hardness Pair Interaction")
dev.off()

png("HPI12_2d_ak_pm7.png",width = 9, height = 6, units = 'in', res = 600)
xyplot(HPI2~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP18)",ylab="Hardness Pair Interaction")
dev.off()


xyplot(SPI1~n|m,type=c("b","g"))
xyplot(SPI2~m|n,type=c("b","g"))

xyplot(CT1~Coordinate_1|Coordinate_2,type=c("b","g"))
xyplot(CT2~Coordinate_2|Coordinate_1,type=c("b","g"))

xyplot(eas_a1~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="EAS O1(ADN)")
xyplot(nas_a1~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="NAS O1(ADN)")
xyplot(nas_a2~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="NAS P1(ATP)")
xyplot(eas_a2~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="EAS P1(ATP)")

xyplot(eas_a3~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP)",ylab="EAS OD2(ASP)")
xyplot(nas_a3~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP)",ylab="NAS OD2(ASP)")
xyplot(nas_a4~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP)",ylab="NAS H1(ADN)")
xyplot(eas_a4~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP)",ylab="EAS H1(ADN)")


xyplot(chg_a1~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="Parital Charge O1(ADN)")
xyplot(chg_a2~n|m,type=c("b","g"),xlab="P1(ATP)--O1(ADN)",ylab="Partial charge P1(ATP)")
xyplot(chg_a3~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP)",ylab="Partial Charge OD2(ASP)")
xyplot(chg_a4~m|n,type=c("b","g"),xlab="H1(ADN)--OD2(ASP)",ylab="Partial Charge H1(ADN)")

#para a primeira coordenada pegar o frame 3 da coordenada 2
#para a segunda cordenada pegar o frame 23 da coordenada 1 
ak_c1 <-subset(ak_rd,m==3)
ak_c2 <-subset(ak_rd,n==22)

p40 <- ggplot(ak_c1,aes(x=n,y=CT1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame P1(ATP)--O1(ADN)", y = "Charge Transfer")       
p40

p41 <- ggplot(ak_c1,aes(x=n,y=HPI1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame P1(ATP)--O1(ADN)", y = "Hardness Pair Interaction")       
p41

p42 <- ggplot(ak_c1,aes(x=n,y=SPI1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame P1(ATP)--O1(ADN)", y = "Softness Pair Interaction")       
p42

p43 <- ggplot(ak_c1,aes(x=n,y=HOF))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame P1(ATP)--O1(ADN)", y = "Heat of Formation(kcal/mol)")       
p43

#-------------------
fig10 <-ggarrange(p40,p41,p42,p43,
			labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)
png("ak_rd_pm7_1dc1.png",width = 5, height = 5, units = 'in', res = 600)
fig10
dev.off()
#-------------------
p40 <- ggplot(ak_c2,aes(x=m,y=CT2))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Charge Transfer")       
p40

p41 <- ggplot(ak_c2,aes(x=m,y=HPI1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Hardness Pair Interaction")       
p41

p42 <- ggplot(ak_c2,aes(x=m,y=SPI1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Softness Pair Interaction")       
p42

p43 <- ggplot(ak_c2,aes(x=m,y=HOF))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Heat of Formation(kcal/mol)")       
p43

#-------------------
fig11 <-ggarrange(p40,p41,p42,p43,
			labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)
png("ak_rd_pm7_1dc2.png",width = 5, height = 5, units = 'in', res = 600)
fig11
dev.off()


#----------------------------------------------------------------------
# Reactivity Descriptors for AK using am1 1d(tranfers of phosphate group) 
ak_rm1_1d <-read.table("ak_rd_am1",header=T)
ak_rm1_1d_r <- ak_rm1_1d[-22]

p44 <- ggplot(ak_rm1_1d_r,aes(x=n,y=HOF))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Heat Of Formation")       
p44

p45 <- ggplot(ak_rm1_1d_r,aes(x=n,y=HPI1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Hardness Pair Interaction")       
p45

p46 <- ggplot(ak_rm1_1d_r,aes(x=n,y=SPI1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Softness Pair Interaction")       
p46

p47 <- ggplot(ak_rm1_1d_r,aes(x=n,y=CT1))+
	geom_line()+
	geom_point()+
	theme_minimal()+
	labs(title="",
       x="Frame ", y = "Charge Transfer")       
p47


fig12 <-ggarrange(p45,p46,p47,p44,
			labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)
png("ak_rd_rm11d.png",width = 5, height = 5, units = 'in', res = 600)
fig12
dev.off()

