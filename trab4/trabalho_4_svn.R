########################################
# Trabalho 4 - INF-615 SVM
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
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
      idx <-sample(1:nrow(split_data[[i]]), 0.11 * nrow(split_data[[i]]))
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
  print(paste("Modelo: ", idx))
  trainData[[idx]] <- getBalancedData(split_data_train, index = idx)
  
  #svm_tune[[idx]] <- tune(svm, V1 ~ ., data = trainData[[idx]], kernel="radial",
  #                        ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)),
  #                        tunecontrol = tune.control(sampling = "fix"))
  
  
  svm_modelRBF[[idx]] <-svm(V1 ~ .,data = trainData[[idx]],cost = .cost[idx],gamma = .gamma)
  
  #svm_modelLin[[idx]] <-svm(V1 ~ ., kernel = "linear", data = trainData[[idx]])#kernel linear
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

getAcc <- function(svm_model,data){
  predictions <- list(matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1),
                      matrix(0L, nrow = nrow(data), ncol = 1))
  
  for (idx in 1:10) {
    set.seed(42)
    svm.pred  <- predict(svm_model[[idx]], data[, -1])
    svm.pred[svm.pred < 0] <- -1
    svm.pred[svm.pred >= 0] <- 1
    predictions[[idx]] <- predictions[[idx]] + svm.pred
  }
  
  combinedPred <- data.frame(alg1=numeric(nrow(predictions[[1]])),
                            alg2=numeric(nrow(predictions[[2]])),
                            alg3=numeric(nrow(predictions[[3]])),
                            alg4=numeric(nrow(predictions[[4]])),
                            alg5=numeric(nrow(predictions[[5]])),
                            alg6=numeric(nrow(predictions[[6]])),
                            alg7=numeric(nrow(predictions[[7]])),
                            alg8=numeric(nrow(predictions[[8]])),
                            alg9=numeric(nrow(predictions[[9]])),
                            alg0=numeric(nrow(predictions[[10]])))
  
  combinedPred[,"alg1"] <- predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg2"] <- - predictions[[1]] + predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg3"] <- - predictions[[1]] - predictions[[2]] + predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg4"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] + predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg5"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] + predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg6"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] + predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg7"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] + predictions[[7]] - predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg8"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] + predictions[[8]] - predictions[[9]] - predictions[[10]]
  combinedPred[,"alg9"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] + predictions[[9]] - predictions[[10]]
  combinedPred[,"alg0"] <- - predictions[[1]] - predictions[[2]] - predictions[[3]] - predictions[[4]] - predictions[[5]] - predictions[[6]] - predictions[[7]] - predictions[[8]] - predictions[[9]] + predictions[[10]]
  
  finalPred = colnames(combinedPred)[apply(combinedPred, 1, which.max)]
  cm = as.matrix(table(Actual = data$V1, Predicted = finalPred))
  print(cm)
  ACCs <- c()
  for (i in 1:10) {
    ACCs[i] = cm[i,i] / sum(cm[1:10,i])
  }
  
  ACC_final <- sum(ACCs)/10
  ACC_final

}

#getAcc(svm_modelLin,valData)#, "Kernel Linear"
getAcc(svm_modelRBF,valData)#, "Kernel RBF"

library(ggplot2)
ACCNorm <-  data.frame(number = factor(c(1:9, 0, "final", 1:9, 0, "final")),
             ACC = factor(round(c(ACCNorm_train, ACCNorm_val), 2)),
             type = factor(c(rep("treino", 11), rep("validação", 11))))

ggplot(data = ACCNorm, aes(x = number, y = ACC, fill = type , adj = 1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle(label, subtitle = NULL)
