require(ggpubr)
require(lattice)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

lac_rd <-read.delim("lac_rd_smo",header=T)
lac_rd$HPI12_A <- lac_rd$HPI12_A*100
lac_rd$HPI12_C <- lac_rd$HPI12_C*100
lac_rd_pm3 <-subset(lac_rd,method=="PM3")
lac_rd_pm6 <-subset(lac_rd,method=="PM6")
lac_rd_am1 <-subset(lac_rd,method=="AM1")
lac_rd_rm1 <-subset(lac_rd,method=="RM1")
lac_rd_pm7 <-subset(lac_rd,method=="PM7")

p2d_01 <- ggplot(lac_rd_am1, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_01

p2d_02 <- ggplot(lac_rd_rm1, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_02

p2d_03 <- ggplot(lac_rd_pm3, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_03

p2d_04 <- ggplot(lac_rd_pm6, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_04

p2d_05 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Hardness\n(eV)"))
p2d_05

p2d_06 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = ECP)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ECP)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("ECP\n(eV)"))
p2d_06

p2d_07 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = Electrophilicity)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Electrophilicity)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar("Softness\n(eV)"))
p2d_07



png("hardness_am1_lac.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_01
dev.off()
png("hardness_rm1_lac.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_02
dev.off()
png("hardness_pm3_lac.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_03
dev.off()
png("hardness_pm6_lac.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_04
dev.off()
png("hardness_pm7_lac.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_05
dev.off()

p2d_1 <- ggplot(lac_rd_am1, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_1

p2d_2 <- ggplot(lac_rd_rm1, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_2


p2d_3 <- ggplot(lac_rd_pm3, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_3


p2d_4 <- ggplot(lac_rd_pm6, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="contour", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))
p2d_4

p2d_5 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 15,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))

p2d_5

png("hof_lac_am1.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_1
dev.off()
png("hof_lac_rm1.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_2
dev.off()
png("hof_lac_pm3.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_3
dev.off()
png("hof_lac_pm6.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_4
dev.off()
png("hof_lac_pm7.png",width = 3, height = 2.4, units = 'in', res = 1000)
p2d_5
dev.off()
#=======================================================================
fig1 <-ggarrange(p2d_5,p2d_05,p2d_06,p2d_07,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("glob_lac.png",width = 7, height = 5, units = 'in', res = 1400)
fig1
dev.off()


#=======================================================================
p2d_6 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_6


p2d_7 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_7

p2d_8 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_8

p2d_9 <- ggplot(lac_rd_pm7, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_9

fig1 <-ggarrange(p2d_6,p2d_7,p2d_8,p2d_9,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_lac_pm7.png",width = 7, height = 5, units = 'in', res = 1400)
fig1
dev.off()
#-----------------------------------------------------------------------
p2d_10 <- ggplot(lac_rd_pm6, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_10


p2d_11 <- ggplot(lac_rd_pm6, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_11

p2d_12 <- ggplot(lac_rd_pm6, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_12

p2d_13 <- ggplot(lac_rd_pm6, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_13

fig2 <-ggarrange(p2d_10,p2d_11,p2d_12,p2d_13,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_lac_pm6.png",width = 7, height = 5, units = 'in', res = 1400)
fig2
dev.off()
#-----------------------------------------------------------------------
p2d_14 <- ggplot(lac_rd_pm3, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_14


p2d_15 <- ggplot(lac_rd_pm3, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_15

p2d_16 <- ggplot(lac_rd_pm3, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_16

p2d_17 <- ggplot(lac_rd_pm3, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_17

fig3 <-ggarrange(p2d_14,p2d_15,p2d_16,p2d_17,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_lac_pm3.png",width = 7, height = 5, units = 'in', res = 1400)
fig3
dev.off()
#-----------------------------------------------------------------------
p2d_18 <- ggplot(lac_rd_rm1, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_18


p2d_19 <- ggplot(lac_rd_rm1, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_19

p2d_20 <- ggplot(lac_rd_rm1, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_20

p2d_21 <- ggplot(lac_rd_rm1, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_21

fig4 <-ggarrange(p2d_18,p2d_19,p2d_20,p2d_21,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_lac_rm1.png",width = 7, height = 5, units = 'in', res = 1400)
fig4
dev.off()
#----------------------------------------------------------------------
p2d_22 <- ggplot(lac_rd_am1, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_A )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_22


p2d_23 <- ggplot(lac_rd_am1, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_B )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_23

p2d_24 <- ggplot(lac_rd_am1, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_C )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_24

p2d_25 <- ggplot(lac_rd_am1, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =HPI12_D )) +
         stat_contour(bins = 10,color="black") +
         ylab("d H(NAD)-C(Lactate)") +
         xlab("d H(HIS)-O(Lactate)") +
         scale_fill_gradientn(colours=jet.colors(70))+
         guides(fill = guide_colorbar(title = ""))
p2d_25

fig5 <-ggarrange(p2d_22,p2d_23,p2d_24,p2d_25,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("hpi_lac_am1.png",width = 7, height = 5, units = 'in', res = 1400)
fig5
dev.off()
996880571
986679605
#-----------------------------------------------------------------------
sp1 <- ggscatter(lac_rd,x="HOF",y="HPI12_A",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.15,             
                color = "method"   , 
                palette =  c("blue", "orange","purple","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = 0) 
sp1

sp2 <- ggscatter(lac_rd,x="HOF",y="HPI12_B",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.15,             
                color = "method"   , 
                palette =  c("blue", "orange","purple","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction", 
                shape = "method",
                )+
  stat_cor(aes(color = method),  label.x = 40, label.y.npc = "bottom") 
sp2

sp3 <- ggscatter(lac_rd,x="HOF",y="HPI12_C",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.3,             
                color = "method"   , 
                palette =  c("blue", "orange","purple","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = 50) 
sp3

sp4 <- ggscatter(lac_rd,x="HOF",y="HPI12_D",
                add = "reg.line"        ,             
                #conf.int = TRUE         ,  
                size = 0.15,             
                color = "method"   , 
                palette =  c("blue", "orange","purple","green", "red")       , 
                xlab = expression(paste(Delta,"H(kcal/mol)")),
                ylab = "Total Hardenss Pair Interaction", 
                shape = "method",
                )+
  stat_cor(aes(color = method), label.x = 5) 
sp4

png("hpi12_Acor.png",width = 5, height = 5, units = 'in', res = 1000)
sp1
dev.off()
png("hpi12_Bcor.png",width = 5, height = 5, units = 'in', res = 1000)
sp2
dev.off()
png("hpi12_Ccor.png",width = 5, height =5, units = 'in', res = 1000)
sp3
dev.off()
png("hpi12_Dcor.png",width = 5, height = 5, units = 'in', res = 1000)
sp4
dev.off()




