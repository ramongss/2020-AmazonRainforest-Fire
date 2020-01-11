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
library(hht)

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
dataset.decomp <- CEEMD(sig = dataset, trials = 100, noise.amp = 6.4e-07)

plot(dataset.decomp)


### IMF1 = seasonal
### IMF2 = trend
### IMF3 = remainder

IMF1 <- data.frame(dataset.decomp$time.series[,1])
colnames(IMF1) <- "seasonal"

IMF2 <- data.frame(dataset.decomp$time.series[,2])
colnames(IMF2) <- "trend"

IMF3 <- data.frame(dataset.decomp$time.series[,3])
colnames(IMF3) <- "remainder"

## ACF and PACF plots to determine lag
acf(dataset$values, main = "")
pacf(dataset$values, main = "")
auto.arima(dataset$values)

## Lag as inputs
lag <- 4

IMF1.x4 <- IMF1[(lag-3):(dim(IMF1)[1]-lag),1]
IMF1.x3 <- IMF1[(lag-2):(dim(IMF1)[1]-lag+1),1]
IMF1.x2 <- IMF1[(lag-1):(dim(IMF1)[1]-lag+2),1]
IMF1.x1 <- IMF1[(lag):(dim(IMF1)[1]-lag+3),1]
IMF1.y  <- IMF1[(lag+1):(dim(IMF1)[1]),1]

IMF1.lag <- cbind(IMF1.y,IMF1.x1,IMF1.x2,IMF1.x3,IMF1.x4)
colnames(IMF1.lag) <- c("y","x1","x2","x3","x4")

IMF2.x4 <- IMF2[(lag-3):(dim(IMF2)[1]-lag),1]
IMF2.x3 <- IMF2[(lag-2):(dim(IMF2)[1]-lag+1),1]
IMF2.x2 <- IMF2[(lag-1):(dim(IMF2)[1]-lag+2),1]
IMF2.x1 <- IMF2[(lag):(dim(IMF2)[1]-lag+3),1]
IMF2.y  <- IMF2[(lag+1):(dim(IMF2)[1]),1]

IMF2.lag <- cbind(IMF2.y,IMF2.x1,IMF2.x2,IMF2.x3,IMF2.x4)
colnames(IMF2.lag) <- c("y","x1","x2","x3","x4")

IMF3.x4 <- IMF3[(lag-3):(dim(IMF3)[1]-lag),1]
IMF3.x3 <- IMF3[(lag-2):(dim(IMF3)[1]-lag+1),1]
IMF3.x2 <- IMF3[(lag-1):(dim(IMF3)[1]-lag+2),1]
IMF3.x1 <- IMF3[(lag):(dim(IMF3)[1]-lag+3),1]
IMF3.y  <- IMF3[(lag+1):(dim(IMF3)[1]),1]

IMF3.lag <- cbind(IMF3.y,IMF3.x1,IMF3.x2,IMF3.x3,IMF3.x4)
colnames(IMF3.lag) <- c("y","x1","x2","x3","x4")

## Training and Test sets
n <- dim(IMF1.lag)[1]
cut <- 0.7 * n

# IMF1
IMF1.train <- IMF1.lag[1:cut,]
IMF1.test <- tail(IMF1.lag,n-cut)

IMF1.xtrain <- IMF1.train[,-1]
IMF1.ytrain <- IMF1.train[,1]

IMF1.xtest <- IMF1.test[,-1]
IMF1.ytest <- IMF1.test[,1]

# IMF2
IMF2.train <- IMF2.lag[1:cut,]
IMF2.test <- tail(IMF2.lag,n-cut)

IMF2.xtrain <- IMF2.train[,-1]
IMF2.ytrain <- IMF2.train[,1]

IMF2.xtest <- IMF2.test[,-1]
IMF2.ytest <- IMF2.test[,1]

# IMF3
IMF3.train <- IMF3.lag[1:cut,]
IMF3.test <- tail(IMF3.lag,n-cut)

IMF3.xtrain <- IMF3.train[,-1]
IMF3.ytrain <- IMF3.train[,1]

IMF3.xtest <- IMF3.test[,-1]
IMF3.ytest <- IMF3.test[,1]

train <- list(IMF1.train,IMF2.train,IMF3.train)
test  <- list(IMF1.test,IMF2.test,IMF3.test)

save.image("fire-datalag.RData")

# Training and Predictions ------------------------------------------
# load("fire-datalag.RData")
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

IMF1.model <- list()
IMF2.model <- list()
IMF3.model <- list()

pred.IMF1.train <- NA
pred.IMF1.test  <- NA
pred.IMF1       <- NA

pred.IMF2.train <- NA
pred.IMF2.test  <- NA
pred.IMF2       <- NA

pred.IMF3.train <- NA
pred.IMF3.test  <- NA
pred.IMF3       <- NA

### Create progress bar
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = length(model.list), width = 30, style = 3)

for (i in 1:length(model.list)) {
  
  print(model.list[i])
  
  IMF1.model[[i]] <- train(y~., data = train[[1]],
                           method = model.list[i],
                           trControl = control,
                           preProcess = c("center","scale"),
                           tuneLength = 4)
  
  IMF2.model[[i]] <- train(y~.,data = train[[2]],
                           method = model.list[i],
                           trControl = control,
                           preProcess = c("center","scale"),
                           tuneLength = 4)
  
  IMF3.model[[i]] <- train(y~.,data = train[[3]],
                           method = model.list[i],
                           trControl = control,
                           preProcess = c("center","scale"),
                           tuneLength = 4)
  ### Prediction
  
  pred.IMF1.train <- predict(IMF1.model[[i]],train[[1]])
  pred.IMF1.test  <- predict(IMF1.model[[i]],test[[1]])
  pred.IMF1[i]     <- data.frame(c(pred.IMF1.train,pred.IMF1.test))
  
  pred.IMF2.train <- predict(IMF2.model[[i]],train[[2]])
  pred.IMF2.test  <- predict(IMF2.model[[i]],test[[2]])
  pred.IMF2[i]     <- data.frame(c(pred.IMF2.train,pred.IMF2.test))
  
  pred.IMF3.train <- predict(IMF3.model[[i]],train[[3]])
  pred.IMF3.test  <- predict(IMF3.model[[i]],test[[3]])
  pred.IMF3[i]     <- data.frame(c(pred.IMF3.train,pred.IMF3.test))
  
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

Metricas <- matrix(nrow = dim(combs)[1],ncol = 5)
Metricas.train <- matrix(nrow = dim(combs)[1],ncol = 5)

Predicao <- matrix(nrow = n, ncol = dim(combs)[1])

# Cria barra de progresso
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combs)[1], width = 30, style = 3)

for (i in 1:dim(combs)[1]) {
  Predicao[,i] <- pred.IMF1[[combs[i,1]]] + 
    pred.IMF2[[combs[i,2]]] + 
    pred.IMF3[[combs[i,3]]]
  
  Predicao.train <- Predicao[,i][1:cut]
  Predicao.test  <- tail(Predicao[,i],n-cut)
  
  # #Metricas
  pred.RMSE  <- RMSE(Predicao.test, Obs.test)
  pred.RRMSE <- RMSE(Predicao.test, Obs.test)/mean(Predicao.test)
  pred.MAPE  <- mape(Predicao.test, Obs.test)
  pred.R2    <- cor(Predicao.test, Obs.test[,1])^2
  
  pred.RMSE.train  <- RMSE(Predicao.train, Obs.train)
  pred.RRMSE.train <- RMSE(Predicao.train, Obs.train)/mean(Predicao.train)
  pred.MAPE.train  <- mape(Predicao.train, Obs.train)
  pred.R2.train    <- cor(Predicao.train, Obs.train[,1])^2
  
  Metricas.train[i,] <- c(i, pred.RMSE.train,pred.RRMSE.train,
                          pred.MAPE.train,pred.R2.train)
  Metricas[i,] <- c(i,pred.RMSE,pred.RRMSE,pred.MAPE,pred.R2)
  
  
  # Atualiza barra de progresso
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste(round(i/dim(combs)[1]*100, 0),
                                       "% done"))
}

colnames(Metricas) <- c("i","RMSE","RRMSE","MAPE","R2")
colnames(Metricas.train) <- c("i","RMSE","RRMSE","MAPE","R2")


Metricas[which.min(Metricas[,2]),]
Metricas[which.min(Metricas[,3]),]
Metricas[which.min(Metricas[,4]),]
Metricas[which.max(Metricas[,5]),]

Metricas.train[which.min(Metricas.train[,2]),]
Metricas.train[which.min(Metricas.train[,3]),]
Metricas.train[which.min(Metricas.train[,4]),]
Metricas.train[which.max(Metricas.train[,5]),]

#### Melhor resultado i = 819
#### IMF1 - brnn, IMF2 - earth, IMF3 - brnn

save.image("CEEMD-results.RData")


Obs <- data.frame(c(Obs.train[,1],Obs.test[,1]))

Pred <- data.frame(Predicao[,829])

PO(Obs,Pred)
