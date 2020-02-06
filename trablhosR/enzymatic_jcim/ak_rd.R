require(ggpubr)
require(lattice)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

ak_rd <-read.delim("ak_smo",header=T)
ak_rd$HPI12_C <- ak_rd$HPI12_C*100
ak_rd$HPI1_C <- ak_rd$HPI1_C*100
ak_rd_pm3 <-subset(ak_rd,method=="PM3")
ak_rd_pm6 <-subset(ak_rd,method=="PM6")
ak_rd_am1 <-subset(ak_rd,method=="AM1")
ak_rd_pm7 <-subset(ak_rd,method=="PM7")

p2d_01 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_01


p2d_03 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_03

p2d_04 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_04

p2d_05 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_05

p2d_051 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = ECP)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ECP)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_051

p2d_052 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = Electrophilicity)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Electrophilicity)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Softness\n(1/eV)"))
p2d_052




png("hardness_am1_ak.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_01
dev.off()
png("hardness_pm3_ak.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_03
dev.off()
png("hardness_pm6_ak.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_04
dev.off()
png("hardness_pm7_ak.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_05
dev.off()

p2d_1 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_1

p2d_3 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_3


p2d_4 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_4

p2d_5 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 15,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))

p2d_5

fig9 <-ggarrange(p2d_5,p2d_05,p2d_051,p2d_052,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("glob_2d_ak_pm7.png",width = 8, height = 6, units = 'in', res = 600)
fig9
dev.off()


png("hof_ak_am1.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_1
dev.off()
png("hof_ak_pm3.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_3
dev.off()
png("hof_ak_pm6.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_4
dev.off()
png("hof_ak_pm7.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_5
dev.off()
#=======================================================================
p2d_10 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
        ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_10


p2d_11 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
		 ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_11

p2d_12 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_12

p2d_13 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_13

fig2 <-ggarrange(p2d_10,p2d_11,p2d_12,p2d_13,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_ak_pm6.png",width = 7, height = 5, units = 'in', res = 1400)
fig2
dev.off()
#------------------------------------------------------------------------
p2d_10 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
        ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_10


p2d_11 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
		 ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_11

p2d_12 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_12

p2d_13 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_13


png("hpi_ak_pm7A.png",width = 4, height = 3.5, units = 'in', res = 1400)
p2d_10
dev.off()

png("hpi_ak_pm7B.png",width = 4, height = 3.5, units = 'in', res = 1400)
p2d_11
dev.off()

png("hpi_ak_pm7C.png",width = 4, height = 3.5, units = 'in', res = 1400)
p2d_12
dev.off()

png("hpi_ak_pm7D.png",width = 4, height = 3.5, units = 'in', res = 1400)
p2d_13
dev.off()

fig3 <-ggarrange(p2d_10,p2d_11,p2d_12,p2d_13,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_ak_pm7.png",width = 7, height = 5, units = 'in', res = 1400)
fig3
dev.off()
#-----------------------------------------------------------------------
p2d_10 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
        ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_10


p2d_11 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
		 ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_11

p2d_12 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_12

p2d_13 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_13

fig4 <-ggarrange(p2d_10,p2d_11,p2d_12,p2d_13,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_ak_am1.png",width = 7, height = 5, units = 'in', res = 1400)
fig4
dev.off()
#-----------------------------------------------------------------------
p2d_10 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
        ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_10


p2d_11 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
		 ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_11

p2d_12 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_12

p2d_13 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +        
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_13

fig4 <-ggarrange(p2d_10,p2d_11,p2d_12,p2d_13,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_ak_pm3.png",width = 7, height = 5, units = 'in', res = 1400)
fig4
dev.off()

#----------------------------------------------------------------------
p2d_11 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = CT1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT1)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = "Charge\nTransfer\nPair 1"))
p2d_11

p2d_12 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = CT2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT2)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = "Charge\nTransfer\nPair 2"))
p2d_12

p2d_13 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = SPI1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI1)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = "Softness\nPair \nInteraction\nPair 2"))
p2d_13

p2d_14 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = SPI2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI2)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = "Softness\nPair \nInteraction\nPair 2"))
p2d_14


fig4 <-ggarrange(p2d_11,p2d_12,p2d_13,p2d_14,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("ct_spi_pm7_AK.png",width = 7, height = 5, units = 'in', res = 1400)
fig4
dev.off()
#-----------------------------------------------------------------------
sp1 <- ggscatter(ak_rd,x="HOF",y="HPI1_A",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.3,             
                color = "method"   , 
                palette =  c("blue", "orange","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction (Pair 1)", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = 15) 
sp1

sp2 <- ggscatter(ak_rd,x="HOF",y="HPI1_B",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.3,             
                color = "method"   , 
                palette =  c("blue", "orange","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction (Pair 1)", 
                shape = "method",
                )+
  stat_cor(aes(color = method),  label.x =18, label.y.npc = "top") 
sp2

sp3 <- ggscatter(ak_rd,x="HOF",y="HPI1_C",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.3,             
                color = "method"   , 
                palette =  c("blue", "orange","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction (Pair 1)", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = 16) 
sp3

sp4 <- ggscatter(ak_rd,x="HOF",y="HPI1_D",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.3,             
                color = "method"   , 
                palette =  c("blue", "orange","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Hardenss Pair Interaction (Pair 1)", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = -5,label.y.npc = "bottom") 
sp4


sp5<- ggscatter(ak_rd,x="HOF",y="CT1",
                add = "reg.line"        ,             
                #conf.int = TRUE           
                size = 0.15,             
                color = "method"   , 
                palette =  c("blue", "orange","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Charge Transfer Coordinate 1", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = -5) 
sp5


png("hpi1_Acor_ak.png",width = 5, height = 5, units = 'in', res = 1000)
sp1
dev.off()
png("hpi1_Bcor_ak.png",width = 5, height = 5, units = 'in', res = 1000)
sp2
dev.off()
png("hpi1_Ccor_ak.png",width = 5, height = 5, units = 'in', res = 1000)
sp3
dev.off()
png("hpi1_Dcor_ak.png",width = 5, height = 5, units = 'in', res = 1000)
sp4
dev.off()
png("ct_cor_ak.png",width = 5, height = 5, units = 'in', res = 1000)
sp5
dev.off()

#===================================================================
pd_10 <- ggplot(ak_rd_pm7, aes(x = m, y = n, z = eas_a3)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =eas_a3)) +
         stat_contour(bins = 10,color="black") +
         ylab("d P1(ATP)-O1(ADN)") +
         xlab("d H1(ADN)-OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = "EAS\nOD2(ASP)"))
pd_10


png("hpi_eas3.png",width = 5, height = 4, units = 'in', res = 1400)
pd_10
dev.off()
