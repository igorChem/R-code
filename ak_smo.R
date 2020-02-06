require(ggpubr)
require(lattice)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

ak_rd <-read.delim("ak_smo",header=T)
ak_rd_pm3 <-subset(ak_rd,method=="PM3")
ak_rd_pm6 <-subset(ak_rd,method=="PM6")
ak_rd_am1 <-subset(ak_rd,method=="AM1")



p2d_1 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Heat of \nFormation \n(kcal/mol)"))
p2d_1

p2d_2 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Heat of \nFormation \n(kcal/mol)"))
p2d_2


p2d_3 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HOF)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HOF)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "Heat of \nFormation \n(kcal/mol)"))
p2d_3

fig1 <-ggarrange(p2d_1,p2d_2,p2d_3,
          labels = c("A", "B","C"),
          ncol = 1, nrow = 3)

png("ak_hof.png",width = 5, height = 7, units = 'in', res = 400)
fig1
dev.off()

p2d_01 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = ECP	)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ECP)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "ECP(eV) \n(kcal/mol)"))
p2d_01

p2d_02 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = ECP)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ECP)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "ECP(eV) \n(kcal/mol)"))
p2d_02


p2d_03 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = ECP)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ECP)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "ECP(eV) \n(kcal/mol)"))
p2d_03


fig01 <-ggarrange(p2d_01,p2d_02,p2d_03,
          labels = c("A", "B","C"),
          ncol = 1, nrow = 3)

png("ak_ECP.png",width = 4, height = 7, units = 'in', res = 400)
fig01
dev.off()

p2d_04 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = Hardness	)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "ECP(eV) \n(kcal/mol)"))
p2d_04

p2d_05 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "ECP(eV) \n(kcal/mol)"))
p2d_05


p2d_06 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = Hardness)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "ECP(eV) \n(kcal/mol)"))
p2d_06


fig02 <-ggarrange(p2d_04,p2d_05,p2d_06,
          labels = c("A", "B","C"),
          ncol = 1, nrow = 3)

png("ak_hard.png",width = 4, height = 7, units = 'in', res = 400)
fig02
dev.off()

p2d_4 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_A)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total A"))
p2d_4


p2d_5 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_B)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total B"))
p2d_5


p2d_6 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_C)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total C"))
p2d_6


p2d_7 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_D)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total D"))
p2d_7


fig2 <-ggarrange(p2d_4,p2d_5,p2d_6,p2d_7,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("ak_HPI_Am1.png",width = 7, height = 6, units = 'in', res = 400)
fig2
dev.off()


p2d_8 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_A)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total A"))
p2d_8


p2d_9 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_B)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total B"))
p2d_9


p2d_10 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_C)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total C"))
p2d_10


p2d_11 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_D)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total D"))
p2d_11


fig3 <-ggarrange(p2d_8,p2d_9,p2d_10,p2d_11,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("ak_HPI_pm3.png",width = 7, height = 6, units = 'in', res = 400)
fig3
dev.off()

p2d_12 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_A)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_A)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total A"))
p2d_12


p2d_13 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_B)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_B)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total B"))
p2d_13


p2d_14 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_C)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_C)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total C"))
p2d_14


p2d_15 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = HPI12_D)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPI12_D)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "HPI total D"))
p2d_15


fig4 <-ggarrange(p2d_12,p2d_13,p2d_14,p2d_15,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

png("ak_HPI_pm6.png",width = 7, height = 6, units = 'in', res = 400)
fig4
dev.off()


p2d_16 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = CT1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT1)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "CT \n coord1"))
p2d_16


p2d_17 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = CT2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT2)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "CT \n coord 2 "))
p2d_17


p2d_18 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = CT1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT1)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "CT \n coord 1"))
p2d_18


p2d_19 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = CT2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT2)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "CT \n coord 2"))
p2d_19


p2d_20 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = CT1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT1)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "CT \n coord 1"))
p2d_20


p2d_21 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = CT2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = CT2)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "CT \n coord 2"))
p2d_21


fig5 <-ggarrange(p2d_16,p2d_17,p2d_18,p2d_19,p2d_20,p2d_21,
          labels = c("A", "B","C","D","E","F"),
          ncol = 2, nrow = 3)

png("ak_ct1ct2.png",width = 7, height = 8, units = 'in', res = 400)
fig5
dev.off()

p2d_22 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = SPI1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI1)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "SPI \n coord 1"))
p2d_22


p2d_23 <- ggplot(ak_rd_am1, aes(x = m, y = n, z = SPI2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI2)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "SPI \n coord 2"))
p2d_23


p2d_24 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = SPI1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI1)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "SPI \n coord 1"))
p2d_24


p2d_25 <- ggplot(ak_rd_pm3, aes(x = m, y = n, z = SPI2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI2)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "SPI \n coord 2"))
p2d_25

p2d_26 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = SPI1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI1)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "SPI \n coord 1"))
p2d_26


p2d_27 <- ggplot(ak_rd_pm6, aes(x = m, y = n, z = SPI2)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = SPI2)) +
         stat_contour(bins = 20,color="black") +
         ylab("P1(ATP)--O1(ADN)") +
         xlab("H1(ADN)--OD2(ASP)") +
         scale_fill_gradientn(colours=jet.colors(7))+
         #scale_fill_gradient(low="white",high="orange")+
         guides(fill = guide_colorbar(title = "SPI \n coord 2"))
p2d_27

fig6 <-ggarrange(p2d_22,p2d_23,p2d_24,p2d_25,p2d_26,p2d_27,
          labels = c("A", "B","C","D","E","F"),
          ncol = 2, nrow = 3)

png("ak_spi.png",width = 7, height = 8, units = 'in', res = 400)
fig6
dev.off()

