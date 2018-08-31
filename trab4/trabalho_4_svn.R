########################################
# Trabalho 4 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())

#install.packages("neuralnet")
library(neuralnet)

#install.packages("e1071")
library(e1071)

setwd("~/Projects/ComplexData/inf-615/trab4")

# create and process data
source("data_processing.R")
set.seed(42)

getBalancedData <- function(split_data, index) {
  posData <- data.frame()
  negData <- data.frame()
  for (i in 1:10) {
    if (i == index) {
      posData <- split_data[[i]]
    }
    else {
      # get only 10% of data
      idx <- sample(1:nrow(split_data[[i]]), 0.11*nrow(split_data[[i]]))
      negData <- rbind(negData, split_data[[i]][idx,])
    }
  }
  posData$V1 = 1
  negData$V1 = -1
  trainData <- rbind(posData, negData)
  return(trainData)
}

# define the formula
feats <- names(valData)
f <- paste(feats[2:length(feats)],collapse=' + ')
f <- paste('V1 ~',f)
f <- as.formula(f)

ACCNorm_train<-c()
ACCNorm_val<-c()
## svm
for (idx in c(1:10)) {
  trainData <- getBalancedData(split_data_train, index = idx)
  x <- subset(trainData, select=-V1)
  y <- trainData$V1
  svm_model<-svm(x,y, cost = 100, gamma = 1)
  #summary(svm_model)
  
  #acuracia de treino
  pred <- predict(svm_model,x)
  pred[pred>0]<-1
  pred[pred<=0]<--1
  CM = as.matrix(table(pred,y))
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm_train <-c(ACCNorm_train, mean(c(TPR, TNR)))
  
  #acuracia de validacao
  x <- subset(valData, select=-V1)
  y <- valData$V1
  pred <- predict(svm_model,x)
  pred[pred>0]<-1
  pred[pred<=0]<--1
  CM = as.matrix(table(pred,y))
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm_val <-c(ACCNorm_val, mean(c(TPR, TNR)))
  
}
