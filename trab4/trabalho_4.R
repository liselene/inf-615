########################################
# Trabalho 2 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################

#install.packages("neuralnet")
library(neuralnet)

#setwd("~/Documents/Curso - Complex Data/INF-0615/Tarefa1/inf-615/trab4")
setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab4")

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
  
  NN[[i]] <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)
}

############### 1 vs Rest
.f <- function() {
trainData <- getBalancedData(split_data_train, 1)
nrow(trainData)

NN1 <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 2 vs Rest
trainData <- getBalancedData(split_data_train, 2)
nrow(trainData)

NN2 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 3 vs Rest
trainData <- getBalancedData(split_data_train, 3)
nrow(trainData)

NN3 <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 4 vs Rest
trainData <- getBalancedData(split_data_train, 4)
nrow(trainData)

NN4 <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 5 vs Rest
trainData <- getBalancedData(split_data_train, 5)
nrow(trainData)

NN5 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 6 vs Rest
trainData <- getBalancedData(split_data_train, 6)
nrow(trainData)

NN6 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 7 vs Rest
trainData <- getBalancedData(split_data_train, 7)
nrow(trainData)

NN7 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 8 vs Rest
trainData <- getBalancedData(split_data_train, 8)
nrow(trainData)

NN8 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 9 vs Rest
trainData <- getBalancedData(split_data_train, 9)
nrow(trainData)

NN9 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 0 vs Rest
trainData <- getBalancedData(split_data_train, 10)
nrow(trainData)

NN0 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

}
