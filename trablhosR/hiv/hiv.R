library(ggpubr)

dt <-read.table("hiv_eigen_ew",header=T)
attach(dt)
df <-data.frame(dt)
detach(dt)
attach(df)

dt2 <-read.table("res_eas",header=T)
df2 <-data.frame(dt2)
attach(df2)
df21 <-subset(df2,type=="Active")
df22 <-subset(df2,type=="Inactive")
detach(df2)
attach(df21)
df3 <-aggregate(EAS_chg~Residuos,FUN=mean)
detach(df21)
attach(df22)
df4 <-aggregate(EAS_chg~Residuos,FUN=mean)
detach(df22)


dg <-read.table("eas_ean",header=T)
df30 <-data.frame(dg)
attach(df30)


p<-ggplot(df30, aes(Residuos, y=EAS_chg,fill=type)) +
  geom_bar(stat="identity",position=position_dodge())+
                labs(x = "",
				y =expression(paste(Delta,"EAS")))+
				theme_minimal()+
				coord_flip()

png("res_analysis",width = 5, height = 3.5, units = 'in', res = 400)
p
dev.off()

p1 <- ggboxplot(df, x = "Residuos", y = "RAS_chg",
                color = "Residuos",
                shape = "Residuos")

dg <-df[-13,]

png("eas_chgcor",width = 4, height = 4, units = 'in', res = 400)
ggscatter(dg, x = "EAS_chg", y = "deltaG",
          add = "reg.line",                                 
          conf.int = TRUE,                                 
          add.params = list(color = "blue",
                            fill = "lightgray")
          )+
          labs(x = expression(paste(Delta,"EAS")),
			y = expression(paste(Delta,"G(kJ/mol)")))+
  stat_cor(method = "pearson", label.x = 0, label.y = -10) 
dev.off()




  
p1 <- ggboxplot(df, x = "Type", y = "EAS_c",
                color = "Type", palette =c("red","blue"),
                shape = "Type")


p2 <- ggboxplot(df, x = "Type", y = "NAS_c",
                color = "Type", palette =c("red","blue"),
                 shape = "Type")

 
p3 <- ggboxplot(df, x = "Type", y = "Dual_c",
                color = "Type", palette =c("red","blue"),
                 shape = "Type")

 
p4 <- ggboxplot(df, x = "Type", y = "RAS_c",
                color = "Type", palette =c("red","blue"),
                 shape = "Type")
png("EAS_c")
p1
dev.off()
png("NAS_c")
p2
dev.off()
png("Dual_c")
p3
dev.off()
png("RAS_c")
p4
dev.off()


p5 <- ggboxplot(df, x = "Type", y = "EAS_p",
                color = "Type", palette =c("green", "red","blue"),
                shape = "Type")


p6 <- ggboxplot(df, x = "Type", y = "NAS_p",
                color = "Type", palette =c("green", "red","blue"),
                 shape = "Type")

 
p7 <- ggboxplot(df, x = "Type", y = "Dual_p",
                color = "Type", palette =c("green", "red","blue"),
                 shape = "Type")

 
p8 <- ggboxplot(df, x = "Type", y = "RAS_p",
                color = "Type", palette =c("green", "red","blue"),
                 shape = "Type")
png("EAS_p")
p5
dev.off()
png("NAS_p")
p6
dev.off()
png("Dual_p")
p7
dev.off()
png("RAS_p")
p8
dev.off()

png("eas_boxplot.png",width = 4, height = 4, units = 'in', res = 400)

p9 <- ggboxplot(df, x = "Type", y = "EAS_chg",
                color = "Type", palette =c("red","blue"),add = "jitter",
                shape = "Type")+
                labs(y = expression(paste(Delta,"EAS")),
					  x ="" )
					  
png("eas_boxplot.png",width = 4, height = 4, units = 'in', res = 400)
p9					  
dev.off()


p10 <- ggboxplot(df, x = "Type", y = "NAS_chg",
                color = "Type", palette =c("green", "red","blue"),
                 shape = "Type")

 
p11 <- ggboxplot(df, x = "Type", y = "Dual_chg",
                color = "Type", palette =c("green", "red","blue"),
                 shape = "Type")

 
p12 <- ggboxplot(df, x = "Type", y = "RAS_chg",
                color = "Type", palette =c("green", "red","blue"),
                 shape = "Type")
png("EAS_ch")
p9
dev.off()
png("NAS_ch")
p10
dev.off()
png("Dual_ch")
p11
dev.off()
png("RAS_ch")
p12
dev.off()


p13 <- ggboxplot(df, x = "Type", y = "EAS_cl",
                color = "Type", palette =c("#00AFBB", "#FC4E07"),
                shape = "Type")


p14 <- ggboxplot(df, x = "Type", y = "NAS_cl",
                color = "Type", palette =c("#00AFBB", "#FC4E07"),
                 shape = "Type")

 
p15 <- ggboxplot(df, x = "Type", y = "Dual_cl",
                color = "Type", palette =c("#00AFBB", "#FC4E07"),
                 shape = "Type")

 
p16 <- ggboxplot(df, x = "Type", y = "RAS_cl",
                color = "Type", palette =c("#00AFBB", "#FC4E07"),
                 shape = "Type")
png("EAS_cl")
p13
dev.off()
png("NAS_cl")
p14
dev.off()
png("Dual_cl")
p15
dev.off()
png("RAS_cl")
p16
dev.off()

