# Caret packge 
vignette(package = "caret")

library(caret)

#avNNet.default {caret}	R Documentation
# Neural Networks Using Model Averaging

data(BloodBrain)
## Not run: 
modelFit <- avNNet(bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)
modelFit

ann1 <-predict(modelFit, bbbDescr)
plot(logBBB~ann1)


#calibration
#Probability Calibration Plot

#Description
#For classification models, this function creates a ’calibration plot’ that describes how consistent
#model probabilities are with observed event rates.

data(mdrr)
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)] # indentifies the predictor variables that have non significant variance; nesse exemplo ele retira essas variáveis
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .5)]

# findcorrelation: determine highly correlated variables

inTrain <- createDataPartition(mdrrClass) # create data partition object
trainX <- mdrrDescr[inTrain[[1]], ]  
trainY <- mdrrClass[inTrain[[1]]] 

testX <- mdrrDescr[-inTrain[[1]], ]
testY <- mdrrClass[-inTrain[[1]]]

library(MASS) # package com os modelos de classificação usados 

ldaFit <- lda(trainX, trainY) # linear discriminant analysis
qdaFit <- qda(trainX, trainY) # quadratic discriminat analysis
testProbs <- data.frame(obs = testY,
                        lda = predict(ldaFit, testX)$posterior[,1],
                        qda = predict(qdaFit, testX)$posterior[,1])
calibration(obs ~ lda + qda, data = testProbs)
calPlotData <- calibration(obs ~ lda + qda, data = testProbs)
calPlotData
xyplot(calPlotData, auto.key = list(columns = 2))
## End(Not run)

#diff.resamples
#Inferential Assessments About Model Performance
#Description
#Methods for making inferences about differences between models

## Not run:
load(url("http://topepo.github.io/caret/exampleModels.RData"))
resamps <- resamples(list(CART = rpartFit,
                          CondInfTree = ctreeFit,
                          MARS = earthFit))
difs <- diff(resamps)
difs
summary(difs)
compare_models(rpartFit, ctreeFit)
## End(Not run)

#caretFuncs
# Backwards Feature Selection Helper Functions

## For picking subset sizes:
## Minimize the RMSE
example <- data.frame(RMSE = c(1.2, 1.1, 1.05, 1.01, 1.01, 1.03, 1.00),
Variables = 1:7)
## Percent Loss in performance (positive)
example$PctLoss <- (example$RMSE - min(example$RMSE))/min(example$RMSE)*100


xyplot(RMSE ~ Variables, data= example)
xyplot(PctLoss ~ Variables, data= example)
absoluteBest <- pickSizeBest(example, metric = "RMSE", maximize = FALSE)
within5Pct <- pickSizeTolerance(example, metric = "RMSE", maximize = FALSE)
cat("numerically optimal:",
example$RMSE[absoluteBest],
"RMSE in position",
absoluteBest, "\n")

cat("Accepting a 1.5 pct loss:",
example$RMSE[within5Pct],
"RMSE in position",
within5Pct, "\n")

## Example where we would like to maximize
example2 <- data.frame(Rsquared = c(0.4, 0.6, 0.94, 0.95, 0.95, 0.95, 0.95),
Variables = 1:7)
## Percent Loss in performance (positive)
example2$PctLoss <- (max(example2$Rsquared) - example2$Rsquared)/max(example2$Rsquared)*100
xyplot(Rsquared ~ Variables, data= example2)
xyplot(PctLoss ~ Variables, data= example2)
absoluteBest2 <- pickSizeBest(example2, metric = "Rsquared", maximize = TRUE)
within5Pct2 <- pickSizeTolerance(example2, metric = "Rsquared", maximize = TRUE)
cat("numerically optimal:",
example2$Rsquared[absoluteBest2],
"R^2 in position",
absoluteBest2, "\n")
cat("Accepting a 1.5 pct loss:",
example2$Rsquared[within5Pct2],
"R^2 in position",
within5Pct2, "\n")


# caretSBF
# Selection By Filtering (SBF) Helper Functions

#Ancillary functions for univariate feature selection
#Usage
anovaScores(x, y)
gamScores(x, y)
caretSBF
lmSBF
rfSBF
treebagSBF
ldaSBF
nbSBF




#dummyVars
#Create A Full Set of Dummy Variables
#Description
#dummyVars creates a full set of dummy variables (i.e. less than full rank parameterization)

when <- data.frame(time = c("afternoon", "night", "afternoon",
                            "morning", "morning", "morning",
                            "morning", "afternoon", "afternoon"),
                   day = c("Mon", "Mon", "Mon",
                           "Wed", "Wed", "Fri",
                           "Sat", "Sat", "Fri"))
levels(when$time) <- list(morning="morning",
                          afternoon="afternoon",
                          night="night")
levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                         Fri="Fri", Sat="Sat", Sun="Sun")
## Default behavior:
model.matrix(~day, when)
mainEffects <- dummyVars(~ day + time, data = when)
mainEffects
predict(mainEffects, when[1:3,])
when2 <- when
when2[1, 1] <- NA
predict(mainEffects, when2[1:3,])
predict(mainEffects, when2[1:3,], na.action = na.omit)
interactionModel <- dummyVars(~ day + time + day:time,
                              data = when,
                              sep = ".")
predict(interactionModel, when[1:3,])
noNames <- dummyVars(~ day + time + day:time,
                     data = when,
                     levelsOnly = TRUE)
predict(noNames, when)
interactionModel

#featurePlot
#Wrapper for Lattice Plotting of Predictor Variables


x <- matrix(rnorm(50*5),ncol=5)
y <- factor(rep(c("A", "B"), 25))
trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(x, y, "ellipse")
featurePlot(x, y, "strip", jitter = TRUE)
featurePlot(x, y, "box") # esse é bem legal 
featurePlot(x, y, "pairs")

# gafs.default
# Genetic algorithm feature selection
# Description
# Supervised feature selection using genetic algorithms


set.seed(1)
train_data <- twoClassSim(100, noiseVars = 10)
test_data <- twoClassSim(10, noiseVars = 10)
## A short example
ctrl <- gafsControl(functions = rfGA,
                    method = "cv",
                    number = 3)
rf_search <- gafs(x = train_data[, -ncol(train_data)],
                  y = train_data$Class,
                  iters = 3,
                  gafsControl = ctrl)
rf_search
## End(Not run)

set.seed(1)
train_data <- twoClassSim(100, noiseVars = 10)
test_data <- twoClassSim(10, noiseVars = 10)
## A short example
ctrl <- gafsControl(functions = rfGA,
                    method = "cv",
                    number = 3)
rf_search <- gafs(x = train_data[, -ncol(train_data)],
                  y = train_data$Class,
                  iters = 3,
                  gafsControl = ctrl)
rf_search
## End(Not run)
set.seed(1)
train_data <- twoClassSim(100, noiseVars = 10)
test_data <- twoClassSim(10, noiseVars = 10)
## A short example
ctrl <- gafsControl(functions = rfGA,
                    method = "cv",
                    number = 3)
rf_search <- gafs(x = train_data[, -ncol(train_data)],
                  y = train_data$Class,
                  iters = 3,
                  gafsControl = ctrl)
rf_search
## End(Not run)

modelLookup()

#train
# fit Predictive Models over Different Tuning Parameters
# Description
# This function sets up a grid of tuning parameters for a number of classification and regression
# routines, fits each model and calculates a resampling based performance measure.



#######################################
## Classification Example
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))


knnFit1

knnFit2 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "boot"))
knnFit2


library(MASS)
nnetFit <- train(TrainData, TrainClasses,
                 method = "nnet",
                 preProcess = "range",
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)
nnetFit


## Regression Example
library(mlbench)
data(BostonHousing)
lmFit <- train(medv ~ . + rm:lstat,
               data = BostonHousing,
               method = "lm")

lmFit

library(rpart)
rpartFit <- train(medv ~ .,
                  data = BostonHousing,
                  method = "rpart",
                  tuneLength = 9)

library(klaR)
rdaFit <- train(Species ~ .,
data = iris,
method = "rda",
control = trainControl(method = "cv"))
plot(rdaFit)
plot(rdaFit, plotType = "level")
ggplot(rdaFit) + theme_bw()



# create data partition 
library(mlbench)
 data(Sonar)
set.seed(107)
 inTrain <- createDataPartition(y = Sonar$Class,
                                  ## the outcome data are needed
                                     p = .75,
                                  ## The percentage of data in the
                                      ## training set
                                     list = FALSE)
                               ## The format of the results
  
## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

#pcaNNet.default
#Neural Networks with a Principal Component Step

#Description
#Run PCA on a dataset, then use it in a neural network model

data(BloodBrain)
modelFit <- pcaNNet(bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)
modelFit

summary(modelFit)
fr <-predict(modelFit, bbbDescr[, 1:10])
plot(logBBB~fr)


## detalhes dos modelos para utilizar o train 


#glm {stats}
#Fitting Generalized Linear Models

## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9) # generate factor levels
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts)) # mostra a data.frame 
glm.D93 <- glm(counts ~ outcome+treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)

#lssvm {kernlab}	R Documentation
#Least Squares Support Vector Machine

library(kernlab)
data(iris)
lir <- lssvm(Species~.,data=iris)
ll <-lssvm(Sepal.Length~.,data=iris)
lir
lirr <- lssvm(Species~.,data= iris, reduced = FALSE)
lirr
## Using the kernelMatrix interface
iris <- unique(iris)
rbf <- rbfdot(0.5)
k <- kernelMatrix(rbf, as.matrix(iris[,-5]))
klir <- lssvm(k, iris[, 5])
klir
pre <- predict(klir, k)
plot(iris)
names(iris)
attach(iris)
lre <-lssvm(Species~Sepal.Length*Sepal.Length,data=iris)
lre
plot(pre~Species)
coefficients(klir)
iris



#ksvm

## simple example using the spam data set
data(spam)
## create test and training set
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]
## train a support vector machine
filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter
## predict mail type on the test set
mailtype <- predict(filter,spamtest[,-58])
## Check results
table(mailtype,spamtest[,58])


#krls {KRLS}	R Documentation
#Kernel-based Regularized Least Squares (KRLS)

# non-linear example
# set up data
N <- 200
x1 <- rnorm(N)
x2 <- rbinom(N,size=1,prob=.2)
y <- x1^3 + .5*x2 + rnorm(N,0,.15)
X <- cbind(x1,x2)

# fit model
krlsout <- krls(X=X,y=y)
# summarize marginal effects and contribution of each variable
summary(krlsout)
# plot marginal effects and conditional expectation plots
plot(krlsout)

