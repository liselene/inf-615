########################################
# Trabalho 4 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################

#install.packages("neuralnet")
library(neuralnet)

#install.packages("e1071")
library(e1071)

setwd("~/Documents/Curso - Complex Data/INF-0615/Tarefa1/inf-615/trab4")
#setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab4")
#setwd("~/Projects/ComplexData/inf-615/trab4")

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
  negData$V1 = 0
  trainData <- rbind(posData, negData)
  return(trainData)
}

# define the formula
feats <- names(valData)
f <- paste(feats[2:length(feats)],collapse=' + ')
f <- paste('V1 ~',f)
f <- as.formula(f)

NN <- list()
for (i in 1:10) {
  trainData <- getBalancedData(split_data_train, i)
  print(paste0("Aplying model ", i, ", data size = " , nrow(trainData)))
  
  NN[[i]] <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE, stepmax = 1e6)
}

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
