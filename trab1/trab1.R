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
# we only need 4 columns to represent 5 elements
trainSet$island = as.numeric(trainSet$ocean_proximity == niveis[5])
trainSet$ocean_proximity = NULL

# TODO: precisa arrumar o factor
valSet$hocean = as.numeric(valSet$ocean_proximity == niveis[1])
valSet$near_bay = as.numeric(valSet$ocean_proximity == niveis[2])
valSet$inland = as.numeric(valSet$ocean_proximity == niveis[3])
valSet$near_ocean = as.numeric(valSet$ocean_proximity == niveis[4])
# we only need 4 columns to represent 5 elements
valSet$island = as.numeric(valSet$ocean_proximity == niveis[5])
valSet$ocean_proximity = NULL

# Há exemplos com features sem anotações? 
paste("Sim")

# Como você lidaria com isso?
paste("Remove-se as amostras sem anotação, caso a variavel seja utilizada no treinamento, caso contrário manteria.")  
trainSet <- trainSet[rowSums(is.na(trainSet)) == 0,]
valSet <- valSet[rowSums(is.na(valSet)) == 0,]

correlation<-cor(trainSet[,-9])

## 2- Normalize os dados de modo que eles fiquem todos no mesmo intervalo.
##    Não normalizaremos latitude e longitude, bem como o preço final
trainSet_save <- trainSet

meanTrainFeatures = colMeans(trainSet[,1:8]) #mean of each feature
stdTrainFeatures = apply(trainSet[,1:8], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

# some weird error is happening here, fix it doing column by column
trainSet[,1:8] <- sweep(trainSet[,1:8], 2, meanTrainFeatures, "-")
trainSet[,1:8] = sweep(trainSet[,1:8], 2, stdTrainFeatures, "/")

valSet[,1:8] <- sweep(valSet[,1:8], 2, meanTrainFeatures, "-")
valSet[,1:8] = sweep(valSet[,1:8], 2, stdTrainFeatures, "/")

# Regressão Linear simples (sem os dados discretos)
model_simple = lm(formula = median_house_value ~ ., data=trainSet[,1:9])
summary(model_simple)

valPred = predict(model_simple, valSet[,1:9])
summary(valPred)

MAE_simple = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_simple

# Regressão Linear simples (com os dados discretos)
model_simple = lm(formula = median_house_value ~ ., data=trainSet[,-14])
summary(model_simple)

valPred = predict(model_simple, valSet)
summary(valPred)

MAE_simple = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_simple

# Regressão Linear mais complexa
model_complex = lm(formula = median_house_value ~. 
                   + I(sqrt(latitude^2*longitude^2))  # size and angle(TODO)
                     , data=trainSet)
summary(model_complex)

valPred = predict(model_complex, valSet)
summary(valPred)

MAE_complex = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_complex

# Regressão Linear mais complexa
model_complex = lm(formula = median_house_value ~ I(total_rooms^2)+I(households^2) #
                   , data=trainSet)
summary(model_complex)

valPred = predict(model_complex, valSet)
summary(valPred)

MAE_complex = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_complex

