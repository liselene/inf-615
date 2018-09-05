########################################
# Trabalho 4 - INF-615 SVM
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
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
svm_tune<-list()
trainData <- list()

#parametros do do tune
.cost <- 1
.gamma <- c(0.0625,0.0625,0.00390625,0.00390625,0.00390625,0.00390625,0.00390625,0.0625,0.0625,0.00390625)

## svm
for (idx in c(1:10)) {
  print(paste("Modelo: ", idx))
  trainData[[idx]] <- getBalancedData(split_data_train, index = idx)
  
  #svm_tune[[idx]] <- tune(svm, V1 ~ ., data = trainData[[idx]], kernel="radial",
  #                        ranges=list(gamma = 2^c(-8,-4,0,4), cost = 2^c(-8,-4,-2,0)),
  #                        tunecontrol = tune.control(sampling = "fix"))
  
  svm_modelRBF[[idx]] <-svm(V1 ~ .,data = trainData[[idx]],iter.max = 150,
                            cost = .cost,gamma = .gamma[idx])

  #svm_tune[[idx]] <- tune(svm, V1~., data = trainData[[idx]], kernel = "linear",
  #                 ranges = list(cost = c(0.001, 0.01, 0.1, 1))) #, 5, 10, 100
  
  svm_modelLin[[idx]] <-svm(V1 ~ ., kernel = "linear", data = trainData[[idx]],
                            cost=0.001, scale = FALSE)#kernel linear
}

getPredictions <- function(svm_model, data) {
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
  return(predictions)
}

evaluatePredictions <- function(predictions, label) {
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
  cm = as.matrix(table(Actual = label, Predicted = finalPred))
  
  ACCs <- c()
  for (i in 1:10) {
    ACCs[i] = cm[i,i] / sum(cm[1:10,i])
  }
  return(list(cm, ACCs))
}

##treino
trainData <- do.call("rbind", split_data_train)
# SVM Linear
predictions_train<-getPredictions(svm_modelLin,trainData)
eval_train <- evaluatePredictions(predictions_train, trainData$V1)
ACCs_lin_train<-eval_train[[2]]
ACC_final_lin_train<-mean(ACCs_lin_train)
cm_lin_train<-eval_train[[1]]

# SVM RBF
predictions_train<-getPredictions(svm_modelRBF,trainData)
eval_train <- evaluatePredictions(predictions_train, trainData$V1)
ACCs_rbf_train<-eval_train[[2]]
ACC_final_rbf_train<-mean(ACCs_lin_train)
cm_rbf_train<-eval_train[[1]]

## validacao
# SVM Linear
predictions_val<-getPredictions(svm_modelLin,valData)
eval_val <- evaluatePredictions(predictions_val, valData$V1)
ACCs_lin_val<-eval_val[[2]]
ACC_final_lin_val<-mean(ACCs_lin_val)
cm_lin_val<-eval_val[[1]]

# SVM RBF
predictions_val<-getPredictions(svm_modelRBF,valData)
eval_val <- evaluatePredictions(predictions_val, valData$V1)
ACCs_rbf_val<-eval_val[[2]]
ACC_final_rbf_val<-mean(ACCs_rbf_val)
cm_rbf_val<-eval_val[[1]]

print(paste0("ACC train Linear = ", ACC_final_lin_train))
print(paste0("ACC val Linear= ", ACC_final_lin_val))
print(paste0("ACC train RBF = ", ACC_final_rbf_train))
print(paste0("ACC val RBF = ", ACC_final_rbf_val))


########################################################################################
# OUTPUTS
########################################################################################

cm_rbf_train        # Confusion matrix for train data(kernel rbf)
cm_lin_train        # Confusion matrix for train data(kernel linear)
cm_rbf_val          # Confusion matrix for val data(kernel rbf)
cm_lin_val          # Confusion matrix for val data(kernel linear)
ACCs_rbf_train      # Array with ACC of each number in train data(kernel rbf)
ACCs_lin_train      # Array with ACC of each number in train data(kernel linear)
ACCs_rbf_val        # Array with ACC of each number in val data(kernel rbf)
ACCs_lin_val        # Array with ACC of each number in val data(kernel linear)
ACC_final_rbf_train # Normalized ACC of all numbers for train data(kernel rbf)
ACC_final_lin_train # Normalized ACC of all numbers for train data(kernel linear)
ACC_final_rbf_val   # Normalized ACC of all numbers for val data (kernel rbf)
ACC_final_lin_val   # Normalized ACC of all numbers for val data(kernel linear)
