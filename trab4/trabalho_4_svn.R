########################################
# Trabalho 4 - INF-615 SVM routines
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list = ls())

#install.packages("e1071")
library(e1071)

setwd("~/Projects/ComplexData/inf-615/trab4")

# create and process data
source("data_processing.R")
set.seed(42)

getBalancedData <- function(split_data, index) {
  posData <- data.frame()
  negData <- data.frame()
  set.seed(42)
  for (i in 1:10) {
    if (i == index) {
      posData <- split_data[[i]]
    }
    else {
      # get only 10% of data
      idx <-
        sample(1:nrow(split_data[[i]]), 0.11 * nrow(split_data[[i]]))
      negData <- rbind(negData, split_data[[i]][idx, ])
    }
  }
  posData$V1 = 1
  negData$V1 = -1
  trainData <- rbind(posData, negData)
  return(trainData)
}

set.seed(42)
svm_modelRBF <- list()
svm_modelLin <- list()
#svm_tune<-list()
trainData <- list()

#parametros do do tune
.cost <- c(1, 1, 1, 10, 1, 1, 10, 1, 1, 1)
.gamma <- 0.5

## svm
for (idx in c(1:10)) {
  #idx<-1
  print(paste("Modelo: ", idx))
  trainData[[idx]] <- getBalancedData(split_data_train, index = idx)
  
  #svm_tune[[idx]] <- tune(svm, V1 ~ ., data = trainData[[idx]], kernel="radial",
  #                        ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)),
  #                        tunecontrol = tune.control(sampling = "fix"))
  
  
  svm_modelRBF[[idx]] <-svm(V1 ~ .,data = trainData[[idx]],cost = .cost[idx],gamma = .gamma)
  
  svm_modelLin[[idx]] <-svm(V1 ~ ., kernel = "linear", data = trainData[[idx]])#kernel linear
}
save.image()

getAccuracy <- function(svm_model, label) {
  set.seed(42)
  #acuracia de treino
  ACCNorm_train <- c()
  for (idx in c(1:10)) {
    svm.pred  <- predict(svm_model[[idx]], trainData[[idx]][, -1])
    svm.pred[svm.pred < 0] <- -1
    svm.pred[svm.pred >= 0] <- 1
    CM = table(pred = svm.pred, true = trainData[[idx]][, 1])
    TPR = CM[2, 2] / (CM[2, 2] + CM[2, 1])
    TNR = CM[1, 1] / (CM[1, 1] + CM[1, 2])
    ACCNorm_train <- c(ACCNorm_train, mean(c(TPR, TNR)))
  }
  #print(ACCNorm_train)
  #print(max(ACCNorm_train))
  ACCNorm_train <- c(ACCNorm_train, mean(ACCNorm_train))
  
  #acuracia de validação
  ACCNorm_val <- c()
  for (idx in c(1:10)) {
    svm.pred  <- predict(svm_model[[idx]], valData[, -1])
    svm.pred[svm.pred < 0] <- -1
    svm.pred[svm.pred >= 0] <- 1
    real <- valData$V1
    if (idx == 10) {
      #numero 0 e o modelo 10
      idx <- 0
    }
    real[real != idx] <- -1
    real[real == idx] <- 1
    CM <- table(pred = svm.pred, true = real)
    TPR = CM[2, 2] / (CM[2, 2] + CM[2, 1])
    TNR = CM[1, 1] / (CM[1, 1] + CM[1, 2])
    ACCNorm_val <- c(ACCNorm_val, mean(c(TPR, TNR)))
  }
  #print(ACCNorm_val)
  #print(max(ACCNorm_val))
  ACCNorm_val <- c(ACCNorm_val, mean(ACCNorm_val))
}

getAccuracy(svm_modelLin, "Kernel Linear")
getAccuracy(svm_modelRBF, "Kernel RBF")

#plot accuracy bar graphic  
library(ggplot2)
ACCNorm <-data.frame(number = factor(c(1:9, 0, "media", 1:9, 0, "media")),
                     ACC = factor(round(c(ACCNorm_train, ACCNorm_val), 2)),
                     type = factor(c(rep("treino", 11), rep("validação", 11))))
    
ggplot(data = ACCNorm, aes(x = number,y = ACC,fill = type ,adj = 1)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle(label, subtitle = NULL)

