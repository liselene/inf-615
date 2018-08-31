########################################
# Trabalho 4 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################

#install.packages("neuralnet")
library(neuralnet)

#install.packages("e1071")
library(e1071)

#setwd("~/Documents/Curso - Complex Data/INF-0615/Tarefa1/inf-615/trab4")
setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab4")
#setwd("~/Projects/ComplexData/inf-615/trab4")

# create and process data
source("data_processing.R")
set.seed(42)

# return a list with 5 balanced datasets
getBalancedData <- function(split_data, index) {
  posData <- list()
  negData <- list(data.frame(),data.frame(),data.frame(),data.frame(),data.frame())
  for (i in 1:10) {
    if (i == index) {
      # generate 5 datasets
      posData[[1]] <- split_data[[i]][1:as.integer(nrow(split_data[[i]])*0.2),]
      posData[[2]] <- split_data[[i]][as.integer(nrow(split_data[[i]])*0.2+1):as.integer(0.4*nrow(split_data[[i]])),]
      posData[[3]] <- split_data[[i]][as.integer(nrow(split_data[[i]])*0.4+1):as.integer(0.6*nrow(split_data[[i]])),]
      posData[[4]] <- split_data[[i]][as.integer(nrow(split_data[[i]])*0.6+1):as.integer(0.8*nrow(split_data[[i]])),]
      posData[[5]] <- split_data[[i]][as.integer(nrow(split_data[[i]])*0.8+1):nrow(split_data[[i]]),]
    }
    else {
      # get only 11% of data
      idx <- sample(1:nrow(split_data[[i]]), 0.11*nrow(split_data[[i]]))
      sample_data <- split_data[[i]][idx,]
      negData[[1]] <- rbind(negData[[1]], sample_data[1:as.integer(nrow(sample_data)*0.2),])
      negData[[2]] <- rbind(negData[[2]], sample_data[as.integer(nrow(sample_data)*0.2+1):as.integer(nrow(sample_data)*0.4),])
      negData[[3]] <- rbind(negData[[3]], sample_data[as.integer(nrow(sample_data)*0.4+1):as.integer(nrow(sample_data)*0.6),])
      negData[[4]] <- rbind(negData[[4]], sample_data[as.integer(nrow(sample_data)*0.6+1):as.integer(nrow(sample_data)*0.8),])
      negData[[5]] <- rbind(negData[[5]], sample_data[as.integer(nrow(sample_data)*0.8+1):nrow(sample_data),])
    }
  }
  
  trainData <- list()
  for(i in 1:5) {
    posData[[i]]$V1 = 1
    negData[[i]]$V1 = 0
    trainData[[i]] <- rbind(posData[[i]], negData[[i]])
  }
  
  return(trainData)
}

# define the formula
feats <- names(valData)
f <- paste(feats[2:length(feats)],collapse=' + ')
f <- paste('V1 ~',f)
f <- as.formula(f)

set.seed(42)

NN <- list(list(), list(), list(), list(), list(), list(), list(), list(), list(), list())
for (i in 1:10) {
  set.seed(42)
  trainData <- getBalancedData(split_data_train, i)
  print(paste0("Aplying model ", i, "..."))
  for(j in 1:5) {
    print(paste0("Data size  ", j, " = " , nrow(trainData[[j]])))
    NN[[i]][[j]] <- neuralnet(formula=f, data=trainData[[j]], hidden=c(3,3), linear.output=FALSE, stepmax = 1e6)
  }
}

set.seed(42)

predictions <- list(matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1),
                    matrix(0L, nrow = nrow(valData), ncol = 1))
for (i in 1:10) {
  set.seed(42)
  for (j in 1:5) {
    nnCompute = compute(NN[[i]][[j]], valData[,2:ncol(valData)])
    prediction = nnCompute$net.result
    prediction[prediction < 0.5] = -1
    prediction[prediction >= 0.5] = 1
    predictions[[i]] <- predictions[[i]] + prediction
  }
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
cm = as.matrix(table(Actual = labelVal, Predicted = finalPred))

ACCs <- c()
for (i in 1:10) {
  ACCs[i] = cm[i,i] / sum(cm[1:10,i])
}

ACC_final <- sum(ACCs)/10
ACC_final

.f2 <- function() {
## svm
trainData <- getBalancedData(split_data_train, 1)
svm.model <- svm(f, data = trainData, cost = 100, gamma = 1)
svm.pred  <- predict(svm.model, valData)
CM = as.matrix(table(Actual = valData$V1, Predicted = svm.pred))
}
#TPR = CM[2,2] / (CM[2,2] + CM[2,1])
#TNR = CM[1,1] / (CM[1,1] + CM[1,2])
#ACCNorm = mean(c(TPR, TNR))
