# Starting Code -----------------------------------------------------
## Clear memory
rm(list=ls(all=TRUE))

## Set working directory
setwd("~/Doc/Fire/")

## Load libraries
library(ggplot2)
library(dplyr)
library(Metrics)
library(mlbench)
library(caret)
library(caretEnsemble)
library(e1071)
library(readxl)
library(forecast)

# Data treatment ----------------------------------------------------
## Load data
load("dataset.RData")

## ACF and PACF plots to determine lag
acf(dataset$values, main = "")
pacf(dataset$values, main = "")
auto.arima(dataset$values)

## Lag as inputs
lag <- 5

x5 <- dataset[(lag-4):(dim(dataset)[1]-lag+0),2]
x4 <- dataset[(lag-3):(dim(dataset)[1]-lag+1),2]
x3 <- dataset[(lag-2):(dim(dataset)[1]-lag+2),2]
x2 <- dataset[(lag-1):(dim(dataset)[1]-lag+3),2]
x1 <- dataset[(lag-0):(dim(dataset)[1]-lag+4),2]
y  <- dataset[(lag+1):(dim(dataset)[1]),2]

dataset.lag <- cbind(y,x1,x2,x3,x4,x5)

## Training and Test sets
n <- dim(dataset.lag)[1]
cut <- 0.7 * n

train <- dataset.lag[1:cut,]
test <- tail(dataset.lag,n-cut)

xtrain <- train[,-1]
ytrain <- train[,1]

xtest <- test[,-1]
ytest <- test[,1]

## Saving RData
save.image("fire-datalag5.RData")

# Training and Predictions ------------------------------------------
## Load treated data
# load("fire-datalag1.RData")
set.seed(1234)

control <- trainControl(method="cv", 
                        number=5, 
                        savePredictions='final',
                        verboseIter = FALSE)

# model.list <- c("knn", "earth",
#                 "svmLinear2", "svmRadial", # SVR
#                 "rf", # Random Forest
#                 "gbm", # Tree
#                 "glmboost", "cubist", # Boosting
#                 "brnn", "mlp") # Neural Network

algorithmList <- c("knn",
                   "svmRadial",
                   "glmboost",
                   "cubist")

BaseLearners <- caretList(y~., data = as.data.frame(train), 
                          trControl = control,
                          preProcess = c("center","scale"), 
                          methodList = algorithmList,
                          tuneLength = 5)

MetaLearner <- caretStack(BaseLearners,
                          method = "brnn",
                          trControl = control)

pred.train <- predict(MetaLearner,train)
pred.test  <- predict(MetaLearner,test)
pred       <- c(pred.train,pred.test)

### Metrics
pred.SMAPE.train  <- smape(pred.train, train[,1])
pred.MASE.train   <- mase(pred.train, train[,1])
pred.RRMSE.train  <- RMSE(pred.train, train[,1])/mean(pred.train)
pred.MAPE.train   <- mape(pred.train, train[,1])
pred.R2.train     <- cor(pred.train, train[,1])^2

pred.SMAPE  <- smape(pred.test, test[,1])
pred.MASE   <- mase(pred.test, test[,1])
pred.RRMSE  <- RMSE(pred.test, test[,1])/mean(pred.test)
pred.MAPE   <- mape(pred.test, test[,1])
pred.R2     <- cor(pred.test, test[,1])^2

Metrics.train <- c(pred.SMAPE.train,pred.MASE.train,pred.RRMSE.train,
                   pred.MAPE.train,pred.R2.train)
Metrics       <- c(pred.SMAPE,pred.MASE,pred.RRMSE,pred.MAPE,pred.R2)

Metrics <- rbind(Metrics.train, Metrics)

rownames(Metrics) <- c("train","test")
colnames(Metrics) <- c("SMAPE","MASE","RRMSE","MAPE","R2")

Metrics

save.image("Results-stack-lag1.RData")

# Plots e TEMP ------------------------------------------------------

# load("Results-stack-lag1.RData")
# load("Results-stack-lag2.RData")
# load("Results-stack-lag3.RData")
# load("Results-stack-lag4.RData")
# load("Results-stack-lag5.RData")


source("Plot.R")

Obs <- data.frame(c(train[,1],test[,1]))

Pred <- data.frame(pred)

{
  x11()
  PO(Obs,Pred)
}

