require(ggpubr)
require(lattice)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


sdh_rd <-read.delim("sdh_data",header=T)

#plot dde delta-hof 
p2d_01 <- ggplot(sdh_rd, aes(x = m, y = n, z = ER)) +
		  stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = ER)) + 
		 stat_contour(bins = 6,color="black") +
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar(title = expression(paste(Delta,"H(kcal/mol)"))))

p2d_01

png("sdh_ener.png",width = 3.3, height = 2.2, units = 'in', res = 600)
p2d_01
dev.off()
#======================================================================
											   
p2d_02 <- ggplot(sdh_rd, aes(x = m, y = n, z = HPIP1 )) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPIP1)) +
         stat_contour(bins = 6,color="black") +
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("HPIP1")          )

p2d_02

png("sdh_hpi1.png",width = 3, height = 2, units = 'in', res = 600)
p2d_02
dev.off()

#======================================================================

p2d_03 <- ggplot(sdh_rd, aes(x = m, y = n, z = HPIP2 )) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = HPIP2)) +
		  stat_contour(bins = 6,color="black") +
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("HPIP2"))

p2d_03

png("sdh_hpi2.png",width = 3, height = 2, units = 'in', res = 600)
p2d_03
dev.off()

#======================================================================

p2d_04 <- ggplot(sdh_rd, aes(x = m, y = n, z = HPIP2+ HPIP1)) +
         stat_contour( geom="polygon",aes(fill = ..level..))+
         geom_tile(aes(fill = HPIP2+HPIP1)) +
		 stat_contour(bins = 6,color="black") +
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("HPI"))

 p2d_04

png("sdh_hpi_t.png",width = 3, height = 2, units = 'in', res = 600)
p2d_04
dev.off()

#======================================================================

p2d_05 <- ggplot(sdh_rd, aes(x = m, y = n, z = Hardness_a1)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness_a1)) +
		 stat_contour(bins = 6,color="black") +         
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("Hardness \n HZ3(L)"))

p2d_05

png("sdh_hard1.png",width = 3, height = 2, units = 'in', res = 600)
p2d_05
dev.off()

#======================================================================

p2d_06 <- ggplot(sdh_rd, aes(x = m, y = n, z = Hardness_a2)) +
         stat_contour( geom="polygon",aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness_a2)) +
		 stat_contour(bins = 6,color="black") +                
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("Hardness \n O3(SHK)"))

p2d_06

png("sdh_hard2.png",width = 3, height = 2, units = 'in', res = 600)
p2d_06
dev.off()


#======================================================================

p2d_07 <- ggplot(sdh_rd, aes(x = m, y = n, z = Hardness_a3)) +
         stat_contour( geom="polygon",aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness_a3)) +
		  stat_contour(bins = 6,color="black") + 
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("Hardness \n 2H3(NAD)"))

p2d_07

png("sdh_hard3.png",width = 3, height = 2, units = 'in', res = 600)
p2d_07
dev.off()

#======================================================================

p2d_08 <- ggplot(sdh_rd, aes(x = m, y = n, z = Hardness_4)) +
         stat_contour( geom="polygon",aes(fill = ..level..))+
         geom_tile(aes(fill = Hardness_4)) +
			stat_contour(bins = 6,color="black") + 
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("Hardness \n C3(SHK)"))

p2d_08

png("sdh_hard4.png",width = 3, height = 2, units = 'in', res = 600)
p2d_08
dev.off()

#======================================================================

p2d_09 <- ggplot(sdh_rd, aes(x = m, y = n, z = EAS_a2)) +
         stat_contour( geom="polygon",aes(fill = ..level..))+
         geom_tile(aes(fill =EAS_a2 )) +
		  stat_contour(bins = 6,color="black") + 
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("EAS \n O3(SHK)"))

p2d_09

png("sdh_eas2.png",width = 3, height = 2, units = 'in', res = 600)
p2d_09
dev.off()

#======================================================================

p2d_10 <- ggplot(sdh_rd, aes(x = m, y = n, z = EAS_a3)) +
         stat_contour(geom="polygon", aes(fill = ..level..))+
         geom_tile(aes(fill =EAS_a3 )) +
						   stat_contour(bins = 6,color="black") + 
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("EAS \n 2H3(NAD)"))

p2d_10

png("sdh_eas3.png",width = 3, height = 2, units = 'in', res = 600)
p2d_10
dev.off()

#======================================================================
p2d_11 <- ggplot(sdh_rd, aes(x = m, y = n, z = NAS_4)) +
         stat_contour( geom="polygon",aes(fill = ..level..))+
         geom_tile(aes(fill =NAS_4 )) +
		   stat_contour(bins = 6,color="black") + 
         xlab("d HZ3(L)-O(SHK)") +
         ylab("d 2H3(NAD)-C(SHK)") +
         scale_fill_gradientn(colours=jet.colors(70))+
		 guides(fill = guide_colorbar("EAS \n C3(SHK) "))

p2d_11

png("sdh_nas4.png",width = 3, height = 2, units = 'in', res = 600)
p2d_11
dev.off()
#======================================================================
