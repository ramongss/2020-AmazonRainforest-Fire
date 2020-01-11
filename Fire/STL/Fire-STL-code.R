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

## Dataset to Time-Series
dataset.ts <- ts(dataset$values, 
                 start = c(1998,6), 
                 end = c(2019,8), 
                 frequency = 12)

dataset.ts

## Decompose dataset
dataset.decomp <- stl(dataset.ts, "per")

plot(dataset.decomp)

### c1 = seasonal
### c2 = trend
### c3 = remainder

c1 <- data.frame(dataset.decomp$time.series[,1])
colnames(c1) <- "seasonal"

c2 <- data.frame(dataset.decomp$time.series[,2])
colnames(c2) <- "trend"

c3 <- data.frame(dataset.decomp$time.series[,3])
colnames(c3) <- "remainder"

## ACF and PACF plots to determine lag
acf(dataset$values, main = "")
pacf(dataset$values, main = "")
auto.arima(dataset$values)

## Lag as inputs
lag <- 5

c1.x5 <- c1[(lag-4):(dim(c1)[1]-lag+0),1]
c1.x4 <- c1[(lag-3):(dim(c1)[1]-lag+1),1]
c1.x3 <- c1[(lag-2):(dim(c1)[1]-lag+2),1]
c1.x2 <- c1[(lag-1):(dim(c1)[1]-lag+3),1]
c1.x1 <- c1[(lag-0):(dim(c1)[1]-lag+4),1]
c1.y  <- c1[(lag+1):(dim(c1)[1]),1]

c1.lag <- cbind(c1.y,c1.x1,c1.x2,c1.x3,c1.x4,c1.x5)
colnames(c1.lag) <- c("y","x1","x2","x3","x4","x5")

c2.x5 <- c2[(lag-4):(dim(c2)[1]-lag+0),1]
c2.x4 <- c2[(lag-3):(dim(c2)[1]-lag+1),1]
c2.x3 <- c2[(lag-2):(dim(c2)[1]-lag+2),1]
c2.x2 <- c2[(lag-1):(dim(c2)[1]-lag+3),1]
c2.x1 <- c2[(lag-0):(dim(c2)[1]-lag+4),1]
c2.y  <- c2[(lag+1):(dim(c2)[1]),1]

c2.lag <- cbind(c2.y,c2.x1,c2.x2,c2.x3,c2.x4,c2.x5)
colnames(c2.lag) <- c("y","x1","x2","x3","x4","x5")

c3.x5 <- c3[(lag-4):(dim(c3)[1]-lag+0),1]
c3.x4 <- c3[(lag-3):(dim(c3)[1]-lag+1),1]
c3.x3 <- c3[(lag-2):(dim(c3)[1]-lag+2),1]
c3.x2 <- c3[(lag-1):(dim(c3)[1]-lag+3),1]
c3.x1 <- c3[(lag-0):(dim(c3)[1]-lag+4),1]
c3.y  <- c3[(lag+1):(dim(c3)[1]),1]

c3.lag <- cbind(c3.y,c3.x1,c3.x2,c3.x3,c3.x4,c3.x5)
colnames(c3.lag) <- c("y","x1","x2","x3","x4","x5")

## Training and Test sets
n <- dim(c1.lag)[1]
cut <- 0.7 * n

# c1
c1.train <- c1.lag[1:cut,]
c1.test <- tail(c1.lag,n-cut)

c1.xtrain <- c1.train[,-1]
c1.ytrain <- c1.train[,1]

c1.xtest <- c1.test[,-1]
c1.ytest <- c1.test[,1]

# c2
c2.train <- c2.lag[1:cut,]
c2.test <- tail(c2.lag,n-cut)

c2.xtrain <- c2.train[,-1]
c2.ytrain <- c2.train[,1]

c2.xtest <- c2.test[,-1]
c2.ytest <- c2.test[,1]

# c3
c3.train <- c3.lag[1:cut,]
c3.test <- tail(c3.lag,n-cut)

c3.xtrain <- c3.train[,-1]
c3.ytrain <- c3.train[,1]

c3.xtest <- c3.test[,-1]
c3.ytest <- c3.test[,1]

train <- list(c1.train,c2.train,c3.train)
test  <- list(c1.test,c2.test,c3.test)

save.image("fire-STL-datalag5.RData")

# Training and Predictions ------------------------------------------
# load("fire-STL-datalag5.RData")
set.seed(1234)

control <- trainControl(method = "timeslice",
                        initialWindow = 0.7*dim(train[[1]])[1],
                        horizon = 1,
                        fixedWindow = FALSE,
                        allowParallel = TRUE,
                        savePredictions = 'final',
                        verboseIter = FALSE)

model.list <- c("knn", "earth",
                "svmLinear2", "svmRadial", # SVR
                "rf", # Random Forest
                "gbm", # Tree
                "glmboost", "cubist", # Boosting
                "brnn", "mlp") # Neural Network

c1.model <- list()
c2.model <- list()
c3.model <- list()

pred.c1.train <- NA
pred.c1.test  <- NA
pred.c1       <- NA

pred.c2.train <- NA
pred.c2.test  <- NA
pred.c2       <- NA

pred.c3.train <- NA
pred.c3.test  <- NA
pred.c3       <- NA

### Create progress bar
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = length(model.list), width = 30, style = 3)

for (i in 1:length(model.list)) {
  
  c1.model[[i]] <- train(y~., data = train[[1]],
                         method = model.list[i],
                         trControl = control,
                         preProcess = c("center","scale"),
                         tuneLength = 5)
  
  c2.model[[i]] <- train(y~.,data = train[[2]],
                         method = model.list[i],
                         trControl = control,
                         preProcess = c("center","scale"),
                         tuneLength = 5)
  
  c3.model[[i]] <- train(y~.,data = train[[3]],
                         method = model.list[i],
                         trControl = control,
                         preProcess = c("center","scale"),
                         tuneLength = 5)
  ### Prediction
  
  pred.c1.train <- predict(c1.model[[i]],train[[1]])
  pred.c1.test  <- predict(c1.model[[i]],test[[1]])
  pred.c1[i]     <- data.frame(c(pred.c1.train,pred.c1.test))
  
  pred.c2.train <- predict(c2.model[[i]],train[[2]])
  pred.c2.test  <- predict(c2.model[[i]],test[[2]])
  pred.c2[i]     <- data.frame(c(pred.c2.train,pred.c2.test))
  
  pred.c3.train <- predict(c3.model[[i]],train[[3]])
  pred.c3.test  <- predict(c3.model[[i]],test[[3]])
  pred.c3[i]     <- data.frame(c(pred.c3.train,pred.c3.test))
  
  ### Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste( round(i/length(model.list)*100, 0),
                                        "% done"))
}

close(pb)

count <- c(1:length(model.list))

combs <- expand.grid(count,count,count)

Obs.train <- train[[1]] + train[[2]] + train[[3]]
Obs.test  <- test[[1]] + test[[2]] + test[[3]]

Metricas <- matrix(nrow = dim(combs)[1],ncol = 6)
Metricas.train <- matrix(nrow = dim(combs)[1],ncol = 6)

Predicao <- matrix(nrow = n, ncol = dim(combs)[1])

### Create progress bar
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combs)[1], width = 30, style = 3)

for (i in 1:dim(combs)[1]) {
  Predicao[,i] <- pred.c1[[combs[i,1]]] + 
    pred.c2[[combs[i,2]]] + 
    pred.c3[[combs[i,3]]]
  
  Predicao.train <- Predicao[,i][1:cut]
  Predicao.test  <- tail(Predicao[,i],n-cut)
  
  # #Metricas
  pred.SMAPE  <- smape(Predicao.test, Obs.test)
  pred.MASE  <- mase(Predicao.test, Obs.test)
  pred.RRMSE <- RMSE(Predicao.test, Obs.test)/mean(Predicao.test)
  pred.MAPE  <- mape(Predicao.test, Obs.test)
  pred.R2    <- cor(Predicao.test, Obs.test[,1])^2
  
  pred.SMAPE.train  <- smape(Predicao.train, Obs.train)
  pred.MASE.train  <- mase(Predicao.train, Obs.train)
  pred.RRMSE.train <- RMSE(Predicao.train, Obs.train)/mean(Predicao.train)
  pred.MAPE.train  <- mape(Predicao.train, Obs.train)
  pred.R2.train    <- cor(Predicao.train, Obs.train[,1])^2
  
  Metricas.train[i,] <- c(i, pred.SMAPE.train,pred.MASE.train,pred.RRMSE.train,
                           pred.MAPE.train,pred.R2.train)
  Metricas[i,] <- c(i,pred.SMAPE,pred.MASE,pred.RRMSE,pred.MAPE,pred.R2)
  
  
  ### Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste(round(i/dim(combs)[1]*100, 0),
                                       "% done"))
}

colnames(Metricas) <- c("i","SMAPE","MASE","RRMSE","MAPE","R2")
colnames(Metricas.train) <- c("i","SMAPE","MASE","RRMSE","MAPE","R2")

# Metricas[which.min(Metricas[,2]),]
# Metricas[which.min(Metricas[,3]),]
# Metricas[which.min(Metricas[,4]),]
# Metricas[which.min(Metricas[,5]),]
# Metricas[which.max(Metricas[,6]),]

save.image("Results-STL-lag5.RData")

# Plots e TEMP ------------------------------------------------------

# load("Results-STL-lag1.RData")
# load("Results-STL-lag2.RData")
# load("Results-STL-lag3.RData")
# load("Results-STL-lag4.RData")
# load("Results-STL-lag5.RData")


# Lag 4 apresentou melhores resultados

Predicao.min <- data.frame(apply(Predicao, 2, min))

Metricas.best <- rbind(Metricas[which.min(Metricas[,2]),],
                       Metricas[which.min(Metricas[,3]),],
                       Metricas[which.min(Metricas[,4]),],
                       Metricas[which.min(Metricas[,5]),],
                       Metricas[which.max(Metricas[,6]),])

rownames(Metricas.best) <- c("SMAPE(min)",
                             "MASE(min)","RRMSE(min)",
                             "MAPE(min)","R2(max)")

Metricas.best

source("Plot.R")

Obs <- data.frame(c(Obs.train[,1],Obs.test[,1]))

Pred <- data.frame(Predicao[,997])

{
  x11()
  PO(Obs,Pred)
}




