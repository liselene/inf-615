########################################
# Trabalho 4 - INF-615 - validade test data
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
set.seed(42)

source("data_processing.R")

#reading test data
print("Reading CSV...")
data = read.csv("mnist_test.csv", header=FALSE)
print("Applying PCA model...")

# apply PCA or normalization here
#remove "only zero columns"
data_filtered <- data[,c(TRUE, colSums(data[,2:ncol(data)]) != 0)]
# apply PCA
data.pca1 <- prcomp(data_filtered[,2:ncol(data_filtered)], scale.=TRUE)

set.seed(42)
# get PCA with 95% of variance
data_test <- data.frame(V1 = data[,1], data.pca1$x[,1:331])

# train the model

# accuracy measure
