########################################
# Trabalho 2 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################

library(neuralnet)

setwd("~/Documents/Curso - Complex Data/INF-0615/Tarefa1/inf-615/trab4")

# create and process data
source("data_processing.R")
set.seed(42)

# define the formula
feats <- names(valData)
f <- paste(feats[2:length(feats)],collapse=' + ')
f <- paste('V1 ~',f)
f <- as.formula(f)

############### 1 vs Rest
positiveTrainData <- split_data_train[[1]]
positiveTrainData$V1 <- 1

negativeTrainData <- do.call("rbind", split_data_train[2:10])
negativeTrainData$V1 <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

idx <- sample(1:nrow(trainData), 0.8*nrow(trainData))
trainDataTest <- trainData[-idx,]
nrow(trainDataTest)

NN1 <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 2 vs Rest
positiveTrainData <- split_data_train[[2]]
positiveTrainData$V1 <- 1

negativeTrainData <- do.call("rbind", append(split_data_train[1], split_data_train[3:10]))
negativeTrainData2$V1 <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

idx <- sample(1:nrow(trainData), 0.8*nrow(trainData))
trainDataTest <- trainData[-idx,]
nrow(trainDataTest)

NN2 = neuralnet(formula=f, data=trainDataTest, hidden=c(3,3), linear.output=FALSE)

############### 3 vs Rest
positiveTrainData <- split_data_train[[3]]
positiveTrainData$V1 <- 1

negativeTrainData <- do.call("rbind", append(split_data_train[1:2], split_data_train[4:10]))
negativeTrainData$V1 <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN3 <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 4 vs Rest
positiveTrainData <- split_data_train[[4]]
positiveTrainData$V1<- 1

negativeTrainData <- do.call("rbind", append(split_data_train[1:3], split_data_train[5:10]))
negativeTrainData$V1 <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN4 <- neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 5 vs Rest
positiveTrainData = split_data_train[[5]]
positiveTrainData$V1 = 1

negativeTrainData = do.call("rbind", append(split_data_train[1:4], split_data_train[6:10]))
negativeTrainData$V1 = 0

trainData = rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN5 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 6 vs Rest
positiveTrainData = split_data_train[[6]]
positiveTrainData$V1 = 1

negativeTrainData = do.call("rbind", append(split_data_train[1:5], split_data_train[7:10]))
negativeTrainData$V1 = 0

trainData = rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN6 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 7 vs Rest
positiveTrainData = split_data_train[[7]]
positiveTrainData$V1 = 1

negativeTrainData = do.call("rbind", append(split_data_train[1:6], split_data_train[8:10]))
negativeTrainData$V1 = 0

trainData = rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN7 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 8 vs Rest
positiveTrainData = split_data_train[[8]]
positiveTrainData$V1 = 1

negativeTrainData = do.call("rbind", append(split_data_train[1:7], split_data_train[9:10]))
negativeTrainData$V1 = 0

trainData = rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN8 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 9 vs Rest
positiveTrainData = split_data_train[[9]]
positiveTrainData$V1 = 1

negativeTrainData = do.call("rbind", append(split_data_train[1:8], split_data_train[10]))
negativeTrainData$V1 = 0

trainData = rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN9 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)

############### 0 vs Rest
positiveTrainData = split_data_train[[10]]
positiveTrainData$V1 = 1

negativeTrainData = do.call("rbind", split_data_train[1:9])
negativeTrainData$V1 = 0

trainData = rbind(positiveTrainData, negativeTrainData)
nrow(trainData)

NN0 = neuralnet(formula=f, data=trainData, hidden=c(3,3), linear.output=FALSE)


