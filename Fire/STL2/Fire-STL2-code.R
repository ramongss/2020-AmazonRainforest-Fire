# Starting Code -----------------------------------------------------
## Clear memory
rm(list=ls(all=TRUE))

## Set working directory
setwd("~/Doc/Fire/")

BaseDir       <- getwd()
ResultsDir    <- paste(BaseDir, "STL2", sep="/")
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

## Dataset to Time-Series
dataset.ts <- ts(dataset$values, 
                 start = c(1998,6), 
                 end = c(2019,8), 
                 frequency = 12)

dataset.ts

## Decompose dataset
dataset.decomp <- stl(dataset.ts, "per")

dataset.ts%>% stl(s.window='periodic') %>% autoplot(xlab = 'Month') + theme_bw()


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
lag <- 10

c1.x10 <- c1[(lag-9):(dim(c1)[1]-lag+0),1]
c1.x9  <- c1[(lag-8):(dim(c1)[1]-lag+1),1]
c1.x8  <- c1[(lag-7):(dim(c1)[1]-lag+2),1]
c1.x7  <- c1[(lag-6):(dim(c1)[1]-lag+3),1]
c1.x6  <- c1[(lag-5):(dim(c1)[1]-lag+4),1]
c1.x5  <- c1[(lag-4):(dim(c1)[1]-lag+5),1]
c1.x4  <- c1[(lag-3):(dim(c1)[1]-lag+6),1]
c1.x3  <- c1[(lag-2):(dim(c1)[1]-lag+7),1]
c1.x2  <- c1[(lag-1):(dim(c1)[1]-lag+8),1]
c1.x1  <- c1[(lag-0):(dim(c1)[1]-lag+9),1]
c1.y   <- c1[(lag+1):(dim(c1)[1]),1]

c1.lag <- cbind(c1.y,c1.x1,c1.x2,c1.x3,c1.x4,c1.x5,
                c1.x6,c1.x7,c1.x8,c1.x9,c1.x10)
colnames(c1.lag) <- c("y","x1","x2","x3","x4","x5",
                      "x6","x7","x8","x9","x10")

c2.x10 <- c2[(lag-9):(dim(c2)[1]-lag+0),1]
c2.x9  <- c2[(lag-8):(dim(c2)[1]-lag+1),1]
c2.x8  <- c2[(lag-7):(dim(c2)[1]-lag+2),1]
c2.x7  <- c2[(lag-6):(dim(c2)[1]-lag+3),1]
c2.x6  <- c2[(lag-5):(dim(c2)[1]-lag+4),1]
c2.x5  <- c2[(lag-4):(dim(c2)[1]-lag+5),1]
c2.x4  <- c2[(lag-3):(dim(c2)[1]-lag+6),1]
c2.x3  <- c2[(lag-2):(dim(c2)[1]-lag+7),1]
c2.x2  <- c2[(lag-1):(dim(c2)[1]-lag+8),1]
c2.x1  <- c2[(lag-0):(dim(c2)[1]-lag+9),1]
c2.y   <- c2[(lag+1):(dim(c2)[1]),1]

c2.lag <- cbind(c2.y,c2.x1,c2.x2,c2.x3,c2.x4,c2.x5,
                c2.x6,c2.x7,c2.x8,c2.x9,c2.x10)
colnames(c2.lag) <- c("y","x1","x2","x3","x4","x5",
                      "x6","x7","x8","x9","x10")

c3.x10 <- c3[(lag-9):(dim(c3)[1]-lag+0),1]
c3.x9  <- c3[(lag-8):(dim(c3)[1]-lag+1),1]
c3.x8  <- c3[(lag-7):(dim(c3)[1]-lag+2),1]
c3.x7  <- c3[(lag-6):(dim(c3)[1]-lag+3),1]
c3.x6  <- c3[(lag-5):(dim(c3)[1]-lag+4),1]
c3.x5  <- c3[(lag-4):(dim(c3)[1]-lag+5),1]
c3.x4  <- c3[(lag-3):(dim(c3)[1]-lag+6),1]
c3.x3  <- c3[(lag-2):(dim(c3)[1]-lag+7),1]
c3.x2  <- c3[(lag-1):(dim(c3)[1]-lag+8),1]
c3.x1  <- c3[(lag-0):(dim(c3)[1]-lag+9),1]
c3.y   <- c3[(lag+1):(dim(c3)[1]),1]

c3.lag <- cbind(c3.y,c3.x1,c3.x2,c3.x3,c3.x4,c3.x5,
                c3.x6,c3.x7,c3.x8,c3.x9,c3.x10)
colnames(c3.lag) <- c("y","x1","x2","x3","x4","x5",
                      "x6","x7","x8","x9","x10")

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

setwd(ResultsDir)
save.image("fire-STL2-datalag10.RData")

# Training and Predictions ------------------------------------------
# setwd(ResultsDir)
# load("fire-STL2-datalag10.RData")
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
                "rf","qrf", # Random Forest
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
                         preProcess = c("pca"),
                         tuneLength = 5)
  
  c2.model[[i]] <- train(y~.,data = train[[2]],
                         method = model.list[i],
                         trControl = control,
                         preProcess = c("pca"),
                         tuneLength = 5)
  
  c3.model[[i]] <- train(y~.,data = train[[3]],
                         method = model.list[i],
                         trControl = control,
                         preProcess = c("pca"),
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

Metrics <- matrix(nrow = dim(combs)[1],ncol = 6)
Metrics.train <- matrix(nrow = dim(combs)[1],ncol = 6)

Prediction <- matrix(nrow = n, ncol = dim(combs)[1])

### Create progress bar
pb <- txtProgressBar(title = "progress bar", min = 0,
                     max = dim(combs)[1], width = 30, style = 3)

for (i in 1:dim(combs)[1]) {
  Prediction[,i] <- pred.c1[[combs[i,1]]] + 
    pred.c2[[combs[i,2]]] + 
    pred.c3[[combs[i,3]]]
  
  for (j in 1:dim(Prediction)[1]) {
    if (Prediction[j,i] < 0) {
      Prediction[j,i] <- 0
    }
  }
  
  Prediction.train <- Prediction[,i][1:cut]
  Prediction.test  <- tail(Prediction[,i],n-cut)
  
  # #Metrics
  pred.SMAPE  <- smape(Prediction.test, Obs.test[,1])
  pred.MASE  <- mase(Prediction.test, Obs.test[,1])
  pred.RRMSE <- RMSE(Prediction.test, Obs.test[,1])/mean(Prediction.test)
  pred.MAPE  <- mape(Prediction.test, Obs.test[,1])
  pred.R2    <- cor(Prediction.test, Obs.test[,1])^2
  
  pred.SMAPE.train  <- smape(Prediction.train, Obs.train[,1])
  pred.MASE.train  <- mase(Prediction.train, Obs.train[,1])
  pred.RRMSE.train <- RMSE(Prediction.train, Obs.train[,1])/mean(Prediction.train)
  pred.MAPE.train  <- mape(Prediction.train, Obs.train[,1])
  pred.R2.train    <- cor(Prediction.train, Obs.train[,1])^2
  
  Metrics.train[i,] <- c(i, pred.SMAPE.train,pred.MASE.train,pred.RRMSE.train,
                           pred.MAPE.train,pred.R2.train)
  Metrics[i,] <- c(i,pred.SMAPE,pred.MASE,pred.RRMSE,pred.MAPE,pred.R2)
  
  
  ### Update progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i, title=paste(round(i/dim(combs)[1]*100, 0),
                                       "% done"))
}

colnames(Metrics) <- c("i","SMAPE","MASE","RRMSE","MAPE","R2")
colnames(Metrics.train) <- c("i","SMAPE","MASE","RRMSE","MAPE","R2")

save.image("Results-STL2-pca-lag10.RData")


# Prediction steps ahead---------------------------------------------
# setwd(ResultsDir)
# load("Results-STL2-pca-lag10.RData")
y.train <- Obs.train[,1]
x.train <- Obs.train[,-1]

y.test  <- Obs.test[,1]
x.test  <- Obs.test[,-1]

## Recursive prediction

h <- 2
{
  c1.xtrainm <- list()
  c2.xtrainm <- list()
  c3.xtrainm <- list()
  
  c1.xtestm  <- list()
  c3.xtestm  <- list()
  c2.xtestm  <- list()
  
  PTRmo <- list()
  PTEmo <- list()
}

for (m in 1:length(model.list)) {
  c1.xtrainm[[m]] <- as.data.frame(c1.xtrain)
  c2.xtrainm[[m]] <- as.data.frame(c2.xtrain)
  c3.xtrainm[[m]] <- as.data.frame(c3.xtrain)
  
  c1.xtestm[[m]]  <- as.data.frame(c1.xtest)
  c3.xtestm[[m]]  <- as.data.frame(c2.xtest)
  c2.xtestm[[m]]  <- as.data.frame(c3.xtest)
  
  PTRmo[[m]] <- matrix(ncol = 3, nrow = dim(train[[1]])[1])
  PTEmo[[m]] <- matrix(ncol = 3, nrow = dim(test[[1]])[1])

  ### Train
  for(p in 1:dim(train[[1]])[1]){
    if(p%%h !=0){
      PTRmo[[m]][p,1] <- predict(c1.model[[m]],c1.xtrainm[[m]][p,])
      c1.xtrainm[[m]][p+1,1] <- PTRmo[[m]][p,1]
      
      PTRmo[[m]][p,2] <- predict(c2.model[[m]],c2.xtrainm[[m]][p,])
      c2.xtrainm[[m]][p+1,1] <- PTRmo[[m]][p,2]
      
      PTRmo[[m]][p,3] <- predict(c3.model[[m]],c3.xtrainm[[m]][p,])
      c3.xtrainm[[m]][p+1,1] <- PTRmo[[m]][p,3]
    }
    else{
      PTRmo[[m]][p,1] <- predict(c1.model[[m]],c1.xtrainm[[m]][p,])
      PTRmo[[m]][p,2] <- predict(c2.model[[m]],c2.xtrainm[[m]][p,])
      PTRmo[[m]][p,3] <- predict(c3.model[[m]],c3.xtrainm[[m]][p,])
    }
  }
  
  ### Test
  for(p in 1:dim(test[[1]])[1]){
    if(p%%h !=0){
      PTEmo[[m]][p,1] <- predict(c1.model[[m]],c1.xtestm[[m]][p,])
      c1.xtestm[[m]][p+1,1] <- PTEmo[[m]][p,1]
      
      PTEmo[[m]][p,2] <- predict(c2.model[[m]],c2.xtestm[[m]][p,])
      c2.xtestm[[m]][p+1,1] <- PTEmo[[m]][p,2]
      
      PTEmo[[m]][p,3] <- predict(c3.model[[m]],c3.xtestm[[m]][p,])
      c3.xtestm[[m]][p+1,1] <- PTEmo[[m]][p,3]
    }
    else{
      PTEmo[[m]][p,1] <- predict(c1.model[[m]],c1.xtestm[[m]][p,])
      PTEmo[[m]][p,2] <- predict(c2.model[[m]],c2.xtestm[[m]][p,])
      PTEmo[[m]][p,3] <- predict(c3.model[[m]],c3.xtestm[[m]][p,])
    }
  }
  
  cat("Model:", model.list[m], m/length(model.list)*100,"%\n")
}

{
  Metrics2.train <- matrix(nrow = dim(combs)[1],ncol = 3)
  Metrics2       <- matrix(nrow = dim(combs)[1],ncol = 3)
  colnames(Metrics2) <- c("comb","RRMSE","R2")
  
  Pred2step.train <- matrix(nrow = dim(train[[1]])[1], ncol = dim(combs)[1])
  Pred2step.test  <- matrix(nrow = dim(test[[1]])[1], ncol = dim(combs)[1])
  Pred2step <- matrix(nrow = n, ncol = dim(combs)[1])
}

for (c in 1:dim(combs)[1]) {
  ### Recomposing the prediction
  Pred2step.train[,c] <- (PTRmo[[combs[c,1]]][,1] + 
                          PTRmo[[combs[c,2]]][,2] + 
                          PTRmo[[combs[c,3]]][,3])
  
  Pred2step.test[,c] <- (PTEmo[[combs[c,1]]][,1] + 
                         PTEmo[[combs[c,2]]][,2] + 
                         PTEmo[[combs[c,3]]][,3])
  
  ### Avoiding negative values
  for (j in 1:dim(Pred2step.train)[1]) {
    if (Pred2step.train[j,c] < 0) {
      Pred2step.train[j,c] <- 0
    }
  }
  for (j in 1:dim(Pred2step.test)[1]) {
    if (Pred2step.test[j,c] < 0) {
      Pred2step.test[j,c] <- 0
    }
  }
  
  ### Predictions
  Pred2step[,c] <- c(Pred2step.train[,c],Pred2step.test[,c])
  
  pred2.RRMSE <- RMSE(Pred2step.test[,c], Obs.test[,1])/mean(Pred2step.test[,c])
  pred2.R2    <- cor(Pred2step.test[,c], Obs.test[,1])^2
  
  pred2.RRMSE.train <- RMSE(Pred2step.train[,c], Obs.train[,1])/mean(Pred2step.train[,c])
  pred2.R2.train    <- cor(Pred2step.train[,c], Obs.train[,1])^2
  
  Metrics2.train[c,] <- cbind(c,pred2.RRMSE.train,pred2.R2.train)
  Metrics2[c,] <- cbind(c,pred2.RRMSE,pred2.R2)
  
  # cat("Combinacao:", c, c/dim(combs)[1]*100,"% \n")
}


save.image("2step-Results.RData")


# Plots e TEMP ------------------------------------------------------

# setwd(ResultsDir)
# load("Results-STL2-centerscale-lag10.RData")
# load("Results-STL2-pca-lag10.RData")


Prediction.min <- data.frame(apply(Prediction, 2, min))

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
source("PO1.R")
source("PO2.R")
source("PO3.R")

date  <- data.frame(dataset[11:255,1])
Obs   <- data.frame(c(Obs.train[,1],Obs.test[,1]))
Pred  <- data.frame(Prediction[,614])
Pred1 <- data.frame(Prediction[,112])
Pred2 <- data.frame(Prediction[,334])
Pred3 <- data.frame(Prediction[,667])
Pred4 <- data.frame(pred.individual[,2])
Pred5 <- data.frame(pred.individual[,4])
Pred6 <- data.frame(pred.individual[,7])

Pred7 <- data.frame(Pred2step[,908])

{
  x11()
  PO3(Obs,Pred1,Pred2,Pred3,Pred4,Pred5,Pred6,
      "STL-MARS","STL-SVR","STL-GLMBoost",
      "MARS","SVR","GLMBoost")
}

{
  x11()
  PO1(date,Obs,Pred,Pred1,Pred2,Pred3,Pred4,Pred5,Pred6,
      "STL-Ensemble",
      "STL-MARS","STL-SVR","STL-GLMBoost",
      "MARS","SVR","GLMBoost")
}

{
  x11()
  PO2(Obs, Pred, "STL-Ensemble")
}

{
  x11()
  Plot2(date,Obs,Pred,Pred7)
}

Metrics.best <- rbind(Metrics[c(614,112,334,667),c(2,4,6)],
                      Metrics.individual[c(2,4,7),c(2,4,6)])

rownames(Metrics.best) <- c("STL-Ensemble",
                            "STL-MARS",
                            "STL-SVR",
                            "STL-GLMBoost",
                            "MARS",
                            "SVR",
                            "GLMBoost")

Metrics.best.train <- rbind(Metrics.train[c(614,112,334,667),c(2,4,6)],
                      Metrics.train.individual[c(2,4,7),c(2,4,6)])

rownames(Metrics.best.train) <- c("STL-Ensemble",
                                  "STL-MARS",
                                  "STL-SVR",
                                  "STL-GLMBoost",
                                  "MARS",
                                  "SVR",
                                  "GLMBoost")

Metrics.best.train
Metrics.best

tail(pred.individual[,2],n-cut)

e <- matrix(ncol = 7, nrow = length(Obs.test))

e[,1] <- (Obs.test[,1] - (Pred2step.test[,908]))
e[,2] <- (Obs.test[,1] - (Pred2step.test[,1]))
e[,3] <- (Obs.test[,1] - (Pred2step.test[,778]))
e[,4] <- (Obs.test[,1] - (Pred2step.test[,1000]))
e[,5] <- (Obs.test[,1] - PTEmo[,1])
e[,6] <- (Obs.test[,1] - PTEmo[,8])
e[,7] <- (Obs.test[,1] - PTEmo[,10])
  
DMtest <- list()

DMresults <- matrix(nrow = 2, ncol = 6)
rownames(DMresults) <- c("DM","p-value")

for (i in 1:6) {
  DMtest[[i]] <- dm.test(e[,1],e[,i+1], h=2)
  
  DMresults[1,i] <- DMtest[[i]]$statistic
  DMresults[2,i] <- DMtest[[i]]$p.value
}

DMtest1 <- dm.test(e1,e2,h=1)
DMtest2 <- dm.test(e1,e3,h=1)
DMtest3 <- dm.test(e1,e4,h=1)
DMtest4 <- dm.test(e1,e5,h=1)
DMtest5 <- dm.test(e1,e6,h=1)
DMtest6 <- dm.test(e1,e7,h=1)

DMtest1$statistic


DMtest1$p.value
DMtest2$p.value
DMtest3$p.value
DMtest4$p.value
DMtest5$p.value
DMtest6$p.value

{
  x11()
  ggplot(dataset, aes(date, values)) + ylab("Fire spots") + xlab("Date") +
    geom_line(size=1)+ theme_bw(base_size = 22) +
    scale_color_manual(values=c("#0000ff"))
}  
  
  
  