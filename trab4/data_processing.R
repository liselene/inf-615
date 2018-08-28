########################################
# Trabalho 4 - INF-615 - Data treatment
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
set.seed(42)

print("Reading CSV...")

data = read.csv("mnist_trainVal.csv", header=FALSE)

# convert output to factor
#data[,1] <- as.factor(data[,1])
summary(data[,1])

print("Applying PCA model...")

# apply PCA or normalization here
#remove "only zero columns"
data_filtered <- data[,c(TRUE, colSums(data[,2:ncol(data)]) != 0)]
# apply PCA
data.pca1 <- prcomp(data_filtered[,2:ncol(data_filtered)], scale.=TRUE)
cumsum(data.pca1$sdev^2 / sum(data.pca1$sdev^2)) # 95% -> 331

# get PCA with 95% of variance
#data_95_var <- data.frame(V1 = data[,1], data.pca1$x[,1:331])
# get PCA with 60% of variance for testing!
data_60_var <- data.frame(V1 = data[,1], data.pca1$x[,1:64])

print("Splitting data...")

split_data <- function(data) {
  split_data_train <- list()
  split_data_val <- list()
  # split data
  data1 <- data[data$V1 == 1, ]
  idx <- sample(1:nrow(data1), 0.8*nrow(data1))
  split_data_train[[1]] <- data1[idx,]
  split_data_val[[1]] <- data1[-idx,]
  
  data2 <- data[data$V1 == 2, ]
  idx <- sample(1:nrow(data2), 0.8*nrow(data2))
  split_data_train[[2]] <- data2[idx,]
  split_data_val[[2]] <- data2[-idx,]
  
  data3 <- data[data$V1 == 3, ]
  idx <- sample(1:nrow(data3), 0.8*nrow(data3))
  split_data_train[[3]] <- data3[idx,]
  split_data_val[[3]] <- data3[-idx,]
  
  data4 <- data[data$V1 == 4, ]
  idx <- sample(1:nrow(data4), 0.8*nrow(data4))
  split_data_train[[4]] <- data4[idx,]
  split_data_val[[4]] <- data4[-idx,]
  
  data5 <- data[data$V1 == 5, ]
  idx <- sample(1:nrow(data5), 0.8*nrow(data5))
  split_data_train[[5]] <- data5[idx,]
  split_data_val[[5]] <- data5[-idx,]
  
  data6 <- data[data$V1 == 6, ]
  idx <- sample(1:nrow(data6), 0.8*nrow(data6))
  split_data_train[[6]] <- data6[idx,]
  split_data_val[[6]] <- data6[-idx,]
  
  data7 <- data[data$V1 == 7, ]
  idx <- sample(1:nrow(data7), 0.8*nrow(data7))
  split_data_train[[7]] <- data7[idx,]
  split_data_val[[7]] <- data7[-idx,]
  
  data8 <- data[data$V1 == 8, ]
  idx <- sample(1:nrow(data8), 0.8*nrow(data8))
  split_data_train[[8]] <- data8[idx,]
  split_data_val[[8]] <- data8[-idx,]
  
  data9 <- data[data$V1 == 9, ]
  idx <- sample(1:nrow(data9), 0.8*nrow(data9))
  split_data_train[[9]] <- data9[idx,]
  split_data_val[[9]] <- data9[-idx,]
  
  data0 <- data[data$V1 == 0, ]
  idx <- sample(1:nrow(data0), 0.8*nrow(data0))
  split_data_train[[10]] <- data0[idx,]
  split_data_val[[10]] <- data0[-idx,]
  
  # join all validation data together
  valData = do.call("rbind", split_data_val)
  return(list(valData, split_data_train, split_data_val))
}

# split data into train and val
splitted_data <- split_data(data_60_var)
valData <- splitted_data[[1]]
split_data_train <- splitted_data[[2]]
split_data_val <- splitted_data[[3]]






