# Starting Code -----------------------------------------------------
## Clear memory
rm(list=ls(all=TRUE))

## Set working directory
setwd("~/Doc/Fire/")

BaseDir       <- getwd()
ResultsDir    <- paste(BaseDir, "Individual", sep="/")
DataDir       <- paste(BaseDir, "Data", sep="/")

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
library(quantregForest)

# Data treatment ----------------------------------------------------
setwd(DataDir)

## Load data
load("dataset.RData")

## ACF and PACF plots to determine lag
acf(dataset$values, main = "")
pacf(dataset$values, main = "")
auto.arima(dataset$values)

## Lag as inputs
lag <- 10

x10 <- dataset[(lag-9):(dim(dataset)[1]-lag+0),2]
x9  <- dataset[(lag-8):(dim(dataset)[1]-lag+1),2]
x8  <- dataset[(lag-7):(dim(dataset)[1]-lag+2),2]
x7  <- dataset[(lag-6):(dim(dataset)[1]-lag+3),2]
x6  <- dataset[(lag-5):(dim(dataset)[1]-lag+4),2]
x5  <- dataset[(lag-4):(dim(dataset)[1]-lag+5),2]
x4  <- dataset[(lag-3):(dim(dataset)[1]-lag+6),2]
x3  <- dataset[(lag-2):(dim(dataset)[1]-lag+7),2]
x2  <- dataset[(lag-1):(dim(dataset)[1]-lag+8),2]
x1  <- dataset[(lag-0):(dim(dataset)[1]-lag+9),2]
y   <- dataset[(lag+1):(dim(dataset)[1]),2]

dataset.lag <- cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

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
setwd(ResultsDir)
save.image("fire-datalag10.RData")

# Training and Predictions ------------------------------------------
## Load treated data
# setwd(ResultsDir)
# load("fire-datalag-10.RData")
set.seed(1234)

control <- trainControl(method = "timeslice",
                        initialWindow = 0.7*dim(train)[1],
                        horizon = 1,
                        fixedWindow = FALSE,
                        # allowParallel = TRUE,
                        savePredictions = 'final',
                        verboseIter = FALSE)

model.list <- c("knn", "earth",
                "svmLinear2", "svmRadial", # SVR
                "rf","qrf", # Random Forest
                "glmboost", "cubist", # Boosting
                "brnn", "mlp") # Neural Network

model <- list()

pred.train <- NULL
pred.test  <- NULL
pred       <- matrix(nrow = n, ncol = length(model.list))

Metrics       <- matrix(nrow = length(model.list)[1],ncol = 6)
Metrics.train <- matrix(nrow = length(model.list)[1],ncol = 6)

### Create progress bar
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = length(model.list), width = 30, style = 3)

for (i in 1:length(model.list)) {
  
  model[[i]] <- train(y~., data = train,
                      method = model.list[i],
                      trControl = control,
                      preProcess = c("pca"),
                      tuneLength = 5)

  ### Prediction
  
  pred.train <- predict(model[[i]],train)
  pred.test  <- predict(model[[i]],test)
  pred[,i]    <- c(pred.train,pred.test)
  
  for (j in 1:dim(pred)[1]) {
    if (pred[j,i] < 0) {
      pred[j,i] <- 0
    }
  }
  
  ### Metrics
  pred.SMAPE  <- smape(pred.test, test[,1])
  pred.MASE  <- mase(pred.test, test[,1])
  pred.RRMSE <- RMSE(pred.test, test[,1])/mean(pred.test)
  pred.MAPE  <- mape(pred.test, test[,1])
  pred.R2    <- cor(pred.test, test[,1])^2

  pred.SMAPE.train  <- smape(pred.train, train[,1])
  pred.MASE.train  <- mase(pred.train, train[,1])
  pred.RRMSE.train <- RMSE(pred.train, train[,1])/mean(pred.train)
  pred.MAPE.train  <- mape(pred.train, train[,1])
  pred.R2.train    <- cor(pred.train, train[,1])^2

  Metrics.train[i,] <- c(i, pred.SMAPE.train,pred.MASE.train,pred.RRMSE.train,
                          pred.MAPE.train,pred.R2.train)
  Metrics[i,] <- c(i,pred.SMAPE,pred.MASE,pred.RRMSE,pred.MAPE,pred.R2)
  
  ### Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste( round(i/length(model.list)*100, 0),
                                        "% done"))
}

close(pb)

colnames(pred) <- model.list
colnames(Metrics) <- c("i","SMAPE","MASE","RRMSE","MAPE","R2")
rownames(Metrics) <- model.list
colnames(Metrics.train) <- c("i","SMAPE","MASE","RRMSE","MAPE","R2")

# Metrics[which.min(Metrics[,2]),]
# Metrics[which.min(Metrics[,3]),]
# Metrics[which.min(Metrics[,4]),]
# Metrics[which.min(Metrics[,5]),]
# Metrics[which.max(Metrics[,6]),]

setwd(ResultsDir)
save.image("Results-10models-pca-lag10.RData")

# Prediction steps ahead---------------------------------------------
# setwd(ResultsDir)
# load("Results-10models-pca-lag10.RData")

h <- 2
{
  PTRmo     <- matrix(ncol = length(model.list), nrow = dim(train)[1])
  PTEmo     <- matrix(ncol = length(model.list), nrow = dim(test)[1])
  Pred2step <- matrix(ncol = length(model.list), nrow = n)
    colnames(PTRmo) <- model.list
    colnames(PTEmo) <- model.list
  Metrics2.train <- matrix(nrow = length(model.list), ncol = 3)
  Metrics2       <- matrix(nrow = length(model.list), ncol = 3)
    colnames(Metrics2.train) <- c("m","RRMSE","R2") 
    colnames(Metrics2)       <- c("m","RRMSE","R2")
    rownames(Metrics2)       <- model.list
}
for (m in 1:length(model.list)) {
  xtrainm <- as.data.frame(xtrain)
  xtestm  <- as.data.frame(xtest)
  
  ## Train
  for(p in 1:dim(train)[1])
  {
    if(p%%h !=0)
    {
      PTRmo[p,m] <- predict(model[[m]],xtrainm[p,])
      xtrainm[p+1,1] <- PTRmo[p,m]
    }
    else
    {
      PTRmo[p,m] <- predict(model[[m]],xtrainm[p,])
    }
  }
  
  ## Test
  for(p in 1:dim(test)[1])
  {
    if(p%%h !=0)
    {
      PTEmo[p,m] <- predict(model[[m]],xtestm[p,])
      xtestm[p+1,1] <- PTEmo[p,m]
    }
    else
    {
      PTEmo[p,m] <- predict(model[[m]],xtestm[p,])
    }
  }
  
  ### Avoiding negative values
  for (j in 1:dim(PTRmo)[1]) {
    if (PTRmo[j,m] < 0) {
      PTRmo[j,m] <- 0
    }
  }
  for (j in 1:dim(PTEmo)[1]) {
    if (PTEmo[j,m] < 0) {
      PTEmo[j,m] <- 0
    }
  }
  
  Pred2step[,m] <- c(PTRmo[,m],PTEmo[,m])
  
  ### Metrics

  pred2.RRMSE.train <- RMSE(PTRmo[,m], train[,1])/mean(PTRmo[,m])
  pred2.R2.train    <- cor(PTRmo[,m], train[,1])^2
  
  pred2.RRMSE <- RMSE(PTEmo[,m], test[,1])/mean(PTEmo[,m])
  pred2.R2    <- cor(PTEmo[,m], test[,1])^2
  
  
  Metrics2.train[m,] <- c(m,pred2.RRMSE.train,pred2.R2.train)
  Metrics2[m,] <- c(m,pred2.RRMSE,pred2.R2)
  
  cat("Model:", model.list[m], m/length(model.list)*100, "%\n")
}

save.image("2step-results.RData")


# Plots e TEMP ------------------------------------------------------

# load("Results-10models-lag1.RData")
# load("Results-10models-lag2.RData")
# load("Results-10models-lag3.RData")
# load("Results-10models-lag4.RData")
# load("Results-10models-lag5.RData")

Prediction.min <- data.frame(apply(pred, 2, min))

Metrics.best <- rbind(Metrics[which.min(Metrics[,2]),],
                      Metrics[which.min(Metrics[,3]),],
                      Metrics[which.min(Metrics[,4]),],
                      Metrics[which.min(Metrics[,5]),],
                      Metrics[which.max(Metrics[,6]),])

rownames(Metrics.best) <- c("SMAPE(min)",
                            "MASE(min)","RRMSE(min)",
                            "MAPE(min)","R2(max)")

Metrics.best

setwd(BaseDir)

source("Plot.R")

Obs <- data.frame(c(train[,1],test[,1]))

Pred <- data.frame(pred[,7])

{
  x11()
  PO(Obs,Pred)
}



