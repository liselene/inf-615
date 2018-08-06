rm(list=ls())

valSet<-read.csv("housePricing_valSet.csv")
trainSet<-read.csv("housePricing_trainSet.csv")

### 1 - Inspecione os dados. Quantos exemplos você tem?
paste("Exemplos de dados de treinamento:", nrow(trainSet))
paste("Exemplos de dados de validacao:",nrow(valSet))

# Como você irá lidar com as features discretas? 
paste("As features serão mapeadas em disversas colunas de binários")
niveis<-unique(trainSet$ocean_proximity)
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

## 2- Normalize os dados de modo que eles fiquem todos no mesmo intervalo.
##    Não normalizaremos latitude e longitude, bem como o preço final
trainSet_save <- trainSet

meanTrainFeatures = colMeans(trainSet[,3:8]) #mean of each feature
stdTrainFeatures = apply(trainSet[,3:8], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

# some weird error is happening here
trainSet[,3:8] <- sweep(trainSet[,3:8], 2, meanTrainFeatures, "-")
trainSet[,3:8] <- trainSet[,3:8] / stdTrainFeatures

valSet[,3:8] <- sweep(valSet[,3:8], 2, meanTrainFeatures, "-")
valSet[,3:8] <- valSet[,3:8] / stdTrainFeatures

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
