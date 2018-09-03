########################################
# Trabalho 4 - INF-615 - validade test data
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
set.seed(42)

source("data_processing.R")

#reading test data
print("Reading CSV...")
data_test = read.csv("mnist_trainVal.csv", header=FALSE)
print("Applying PCA model...")

# apply PCA or normalization here
#remove "only zero columns"
data_filtered <- data_test[,c(TRUE, colSums(data[,2:ncol(data)]) != 0)]
# apply PCA
data_test <- predict(data.pca1, data_filtered)

set.seed(42)
# get PCA with 95% of variance
data_test <- data.frame(V1 = data[,1], data.pca1$x[,1:332])

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

# train the model
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
    NN[[i]][[j]] <- neuralnet(formula=f, data=trainData[[j]], hidden=c(10,10), linear.output=FALSE, stepmax = 1e6)
  }
}

# accuracy measure
labelTest = data_test[,"V1"]

predictions_test <- getPredictions(NN, data_test)
eval_test <- evaluatePredictions(predictions_test, labelTest)

cm_test <- eval_test[[1]]
ACCs_test <- eval_test[[2]]
ACC_final_test <- sum(eval_test[[2]])/10

