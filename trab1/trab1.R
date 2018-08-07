rm(list=ls())

valSet<-read.csv("housePricing_valSet.csv")
trainSet<-read.csv("housePricing_trainSet.csv")

### 1 - Inspecione os dados. Quantos exemplos você tem?
paste("Exemplos de dados de treinamento:", nrow(trainSet))
paste("Exemplos de dados de validacao:",nrow(valSet))

# Como você irá lidar com as features discretas? 
paste("As features serão mapeadas em disversas colunas de binários")
niveis<-unique(trainSet$ocean_proximity)
levels(valSet$ocean_proximity) <- levels(trainSet$ocean_proximity)

trainSet$hocean = as.numeric(trainSet$ocean_proximity == niveis[1])
trainSet$near_bay = as.numeric(trainSet$ocean_proximity == niveis[2])
trainSet$inland = as.numeric(trainSet$ocean_proximity == niveis[3])
trainSet$near_ocean = as.numeric(trainSet$ocean_proximity == niveis[4])
trainSet$island = as.numeric(trainSet$ocean_proximity == niveis[5])
trainSet$ocean_proximity = NULL

# TODO: precisa arrumar o factor
valSet$hocean = as.numeric(valSet$ocean_proximity == niveis[1])
valSet$near_bay = as.numeric(valSet$ocean_proximity == niveis[2])
valSet$inland = as.numeric(valSet$ocean_proximity == niveis[3])
valSet$near_ocean = as.numeric(valSet$ocean_proximity == niveis[4])
valSet$island = as.numeric(valSet$ocean_proximity == niveis[5])
valSet$ocean_proximity = NULL

# Há exemplos com features sem anotações? 
paste("Sim")

# Como você lidaria com isso?
paste("Remove-se as amostras sem anotação, caso a variavel seja utilizada no treinamento, caso contrário manteria.")  
trainSet <- trainSet[rowSums(is.na(trainSet)) == 0,]
valSet <- valSet[rowSums(is.na(valSet)) == 0,]

## 2- Normalize os dados de modo que eles fiquem todos no mesmo intervalo.
##    Não normalizaremos latitude e longitude, bem como o preço final
trainSet_save <- trainSet

meanTrainFeatures = colMeans(trainSet[,1:8]) #mean of each feature
stdTrainFeatures = apply(trainSet[,1:8], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

# some weird error is happening here, fix it doing column by column
trainSet[,1:8] <- sweep(trainSet[,1:8], 2, meanTrainFeatures, "-")
#trainSet[,1:8] <- trainSet[,1:8] / stdTrainFeatures
trainSet[, 1] <- trainSet[,1] / stdTrainFeatures[1]
trainSet[, 2] <- trainSet[,2] / stdTrainFeatures[2]
trainSet[, 3] <- trainSet[,3] / stdTrainFeatures[3]
trainSet[, 4] <- trainSet[,4] / stdTrainFeatures[4]
trainSet[, 5] <- trainSet[,5] / stdTrainFeatures[5]
trainSet[, 6] <- trainSet[,6] / stdTrainFeatures[6]
trainSet[, 7] <- trainSet[,7] / stdTrainFeatures[7]
trainSet[, 8] <- trainSet[,8] / stdTrainFeatures[8]

valSet[,1:8] <- sweep(valSet[,1:8], 2, meanTrainFeatures, "-")
#valSet[,1:8] <- valSet[,1:8] / stdTrainFeatures
valSet[, 1] <- valSet[,1] / stdTrainFeatures[1]
valSet[, 2] <- valSet[,2] / stdTrainFeatures[2]
valSet[, 3] <- valSet[,3] / stdTrainFeatures[3]
valSet[, 4] <- valSet[,4] / stdTrainFeatures[4]
valSet[, 5] <- valSet[,5] / stdTrainFeatures[5]
valSet[, 6] <- valSet[,6] / stdTrainFeatures[6]
valSet[, 7] <- valSet[,7] / stdTrainFeatures[7]
valSet[, 8] <- valSet[,8] / stdTrainFeatures[8]

# Regressão Linear
# TODO: Aplicar regressão

# nrow(samples)
# ncol(samples)
# 
# nosex<-samples[,2:9]
# summary(nosex)
# set.seed(42)
# train_index<-sample(1:nrow(samples),size=0.8*nrow(samples))
# train<-nosex[train_index,]
# test<-nosex[-train_index,]
# 
# model_simple <- lm(formula=rings ~ length + diameter+height+whole_weight+shucked_weight+viscera_weight+shell_weight,data=train)
# summary(model_simple)
# 
# valPredito <- predict(model_simple,test[1,1:7])
# test[1,"rings"]
# 
# valPredito <- predict(model_simple,test[2,1:7])
# test[2,"rings"]
# 
# valPredito<-predict(model_simple,test[,1:7])
# summary(valPredito)
# 
# #exemplo aula
# model_complex <- lm(formula = rings ~ length + diameter + height + shell_weight + whole_weight + shucked_weight + viscera_weight + I(height^2) + I(length^2) + I(diameter^2) +I(shell_weight^2)+I(whole_weight^2)+I(shucked_weight^2)+I(viscera_weight^2),data=train)
# summary(model_complex)
# valPredito <- predict(model_complex,test[,1:7])
# # MAE
# MAE_simple = sum(abs(valPredito - test$rings)) / length(valPredito)
# MAE_simple
# 
# #meu exemplo
# model_complex <- lm(formula = rings ~ diameter + shell_weight + whole_weight + shucked_weight + viscera_weight + I(length*height), data=train)
# summary(model_complex)
# valPredito <- predict(model_complex,test[,1:7])
# # MAE
# MAE_simple = sum(abs(valPredito - test$rings)) / length(valPredito)
# MAE_simple
# 
# #Normalizacao
# meanTrain <- colMeans(train[,2:8])
# stdTrain <- apply(train[,2:8], 2, sd)
# 
# ################### reorganizar
# meanTrainFeatures
# stdTrainFeatures
# 
# trainData[,2:8] = sweep(trainData[,2:8], 2, meanTrainFeatures, "-")
# trainData[,2:8] = trainData[,2:8] / stdTrainFeatures
# 
# valData[,2:8] = sweep(valData[,2:8], 2, meanTrainFeatures, "-")
# valData[,2:8] = valData[,2:8] / stdTrainFeatures
# 
# trainData[1:5,]
# summary(trainData)
# 
# ## Let's train a linear regression
# model_simple = lm(formula = rings ~ length + diameter + height +
#                 whole_weight + shucked_weight + viscera_weight +
#                 shell_weight,
#                 data=trainData)
# 
# summary(model_simple)
# valPred = predict(model_simple, valData)
# summary(valPred)
