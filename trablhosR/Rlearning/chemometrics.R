# Eloratory data analysis 

## chemometrics package 

library("chemometrics")

#clvalidity, cluster analysis validation based on differences in the 
# sum of squares between and within groups 

clvalidity(x, clnumb = c(2:10))

#example 
data(glass)
require(robustbase)
res <- pcaCV(glass,segments=4,repl=100,cex.lab=1.2,ylim=c(0,1),las=1)

# knnEval kNN evaluation by CV
# Evaluation for k-Nearest-Neighbors (kNN) classification by cross-validation

data(fgl,package="MASS")
grp=fgl$type
X=scale(fgl[,1:9])
k=length(unique(grp))
dat=data.frame(grp,X)
n=nrow(X)
ntrain=round(n*2/3)
require(class)
set.seed(123)
train=sample(1:n,ntrain)
resknn=knnEval(X,grp,train,knnvec=seq(1,30,by=1),legpos="bottomright")
title("kNN classification")


# pcaVarexpl PCA diagnostics for variables

# plots para representar importância das variáveis no PCA 

#exemplo
res <- pcaVarexpl(glass,a=2)


#plotcompmvr Component plot for repeated DCV

#examplo
data(NIR)
X <- NIR$xNIR[1:30,]
# first 30 observations - for illustration
y <- NIR$yGlcEtOH[1:30,1] # only variable Glucose
NIR.Glc <- data.frame(X=X, y=y)
res <- mvr_dcv(y~.,data=NIR.Glc,ncomp=10,method="simpls",repl=10) # Repeated double-cross-validation for PLS and PCR

plot2 <- plotcompmvr(res)


#chemometrics book
# factor analysis

factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA,
         subset, na.action, start = NULL,
         scores = c("none", "regression", "Bartlett"),
         rotation = "varimax", control = NULL, ...)


#cluster analysis 
# função hclust (hierarchical clustering)

hclust(d, method = "complete", members = NULL)
plot(x, labels = NULL, hang = 0.1,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height", ...)

#example 
require(graphics)

hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)








