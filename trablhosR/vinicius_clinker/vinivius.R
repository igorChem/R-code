dt1 <-read.delim("datac",header=T)
attach(dt1)
library(pheatmap)
library("ggcorrplot")
library("PerformanceAnalytics")
attach(dt1)
#---------------------------------------------------------
## correlação entre respostas
my_resp <- dt1[, c(22,23,27,28,29)]
png("respostas.png",units="in",res=400,height=6,width=6)
p1 <-chart.Correlation(my_resp, histogram=TRUE, pch=19)
dev.off()

## correlalçao entre farinha e resposta
my_data <- dt1[, c(22,23,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,52)]
corr <- round(cor(my_data), 1)
# Visualize
png("correlação_farinha.png",units="in",res=400,height=16,width=16)
ggcorrplot(corr, p.mat = cor_pmat(my_data),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)
           
dev.off()
## correlalçao entre dados de produção e resposta
my_data1 <- dt1[, c(1,3,4,5,6,7,8,9,10,12,17,18,19,22,26,27,28,29,31,32,33,35,36,37,38,39)]
corr1 <- round(cor(my_data1), 1)
# Visualize
png("correlação_processo.png",units="in",res=400,height=16,width=16)
ggcorrplot(corr1, p.mat = cor_pmat(my_data1),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)
           
dev.off()
#=======================================================================
#heatmap with clustering
clust_data <- dt1[,c(22,34,44,42,10,7,4,18)]

clust_data %>%
	scale() %>%
	t() %>%
pheatmap(border_color=NA,fontsize=15,main="\n",
filenam="l.pdf",width=12,height=10)

png("correl_alm1.png",units="in",res=400,height=6,width=6)
p1 <-chart.Correlation(clust_data, histogram=TRUE, pch=19)
dev.off()

clust_data <- dt1[,c(22,34,44,42,10,7,4,18)]
#=======================================================================
#krls 
set.seed(107)

dt_1 <- dt1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,40,41,42,43,44,45,46,47,48,49,50,51,52)]
inTrain <-createDataPartition(y=dt_1[,22],
			     p=0.8,
			     list=FALSE)
			     
TrainW <-dt_1[inTrain,]
TestW  <-dt_1[-inTrain,]

krls01 <-krls(X=TrainW[,-23],y=TrainW[,23])

summary(krls01)

predicted <-predict.krls(krls01,newdata=TestW[,-23])
test <-cbind(TestW[23],predicted$fit)

png("predkrls",width = 6, height = 7, units = 'in', res = 125)
plot(fitted(krls01)~TrainW[,23],col="blue",ylab="Alita M3 Estimada",
            xlab="Alita M3 medida",main="modelo KRLS")
            points(TestW[,23],predicted$fit,col="red")
            legend("bottomright",legend=c("Dados de Teste","Dados do modelo"),
            pch=c(1,1),lwd=c(2,2),col=c("red","blue"),cex=.8)
dev.off()

#=======================================================================
dt_1 <- dt1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,29,40,41,42,43,44,45,46,47,48,49,50,51,52)]
inTrain <-createDataPartition(y=dt_1[,29],
			     p=0.75,
			     list=FALSE)
			     
TrainW <-dt_1[inTrain,]
TestW  <-dt_1[-inTrain,]

krls01 <-krls(X=TrainW[,-29],y=TrainW[,29])

summary(krls01)

predicted <-predict.krls(krls01,newdata=TestW[,-29])
test <-cbind(TestW[29],predicted$fit)

png("predkrls3.png",width = 6, height = 7, units = 'in', res = 125)
plot(fitted(krls01)~TrainW[,29],col="blue",ylab="Aluminato Ortorombico Estimada",
            xlab="Aluminato Ortorombico medida",main="modelo KRLS")
            points(TestW[,29],predicted$fit,col="red")
            legend("bottomright",legend=c("Dados de Teste","Dados do modelo"),
            pch=c(1,1),lwd=c(2,2),col=c("red","blue"),cex=.8)
dev.off()
#=======================================================================
dt_1 <- dt1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,28,40,41,42,43,44,45,46,47,48,49,50,51,52)]
inTrain <-createDataPartition(y=dt_1[,28],
			     p=0.75,
			     list=FALSE)
			     
TrainW <-dt_1[inTrain,]
TestW  <-dt_1[-inTrain,]

krls01 <-krls(X=TrainW[,-28],y=TrainW[,28])

summary(krls01)

predicted <-predict.krls(krls01,newdata=TestW[,-28])
test <-cbind(TestW[28],predicted$fit)

png("predkrls4.png",width = 6, height = 7, units = 'in', res = 125)
plot(fitted(krls01)~TrainW[,28],col="blue",ylab="Aluminato Cúbico Estimada",
            xlab="Aluminato Cúbico medida",main="modelo KRLS")
            points(TestW[,28],predicted$fit,col="red")
            legend("bottomright",legend=c("Dados de Teste","Dados do modelo"),
            pch=c(1,1),lwd=c(2,2),col=c("red","blue"),cex=.8)
dev.off()



