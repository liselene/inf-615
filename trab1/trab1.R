rm(list=ls())

valSet<-read.csv("housePricing_testSet.csv")
trainSet<-read.csv("housePricing_trainSet.csv")

# Estou dividindo o data set de treinamento em 2 para testar melhor meus dados
# Testando com o value e test set n??o estou obtendo resultados confi??veis
#set.seed(123)
#train_ind <- sample(seq_len(nrow(trainSet)), size = 0.75*nrow(trainSet))
#valSet <- trainSet[-train_ind,]
#trainSet <- trainSet[train_ind,]

summary(trainSet)
summary(valSet)

### 1 - Inspecione os dados. Quantos exemplos voc?? tem?
paste("Exemplos de dados de treinamento:", nrow(trainSet))
paste("Exemplos de dados de validacao:",nrow(valSet))

# Como voc?? ir?? lidar com as features discretas? 
paste("As features ser??o mapeadas em disversas colunas de bin??rios")
niveis<-unique(trainSet$ocean_proximity)
levels(valSet$ocean_proximity) <- levels(trainSet$ocean_proximity)

trainSet$hocean = as.numeric(trainSet$ocean_proximity == niveis[1])
trainSet$near_bay = as.numeric(trainSet$ocean_proximity == niveis[2])
trainSet$inland = as.numeric(trainSet$ocean_proximity == niveis[3])
trainSet$near_ocean = as.numeric(trainSet$ocean_proximity == niveis[4])
# we only need 4 columns to represent 5 elements
#trainSet$island = as.numeric(trainSet$ocean_proximity == niveis[5])
trainSet$ocean_proximity = NULL

valSet$hocean = as.numeric(valSet$ocean_proximity == niveis[1])
valSet$near_bay = as.numeric(valSet$ocean_proximity == niveis[2])
valSet$inland = as.numeric(valSet$ocean_proximity == niveis[3])
valSet$near_ocean = as.numeric(valSet$ocean_proximity == niveis[4])
# we only need 4 columns to represent 5 elements
#valSet$island = as.numeric(valSet$ocean_proximity == niveis[5])
valSet$ocean_proximity = NULL

# H?? exemplos com features sem anota????es? 
paste("Sim")

# Como voc?? lidaria com isso?
paste("Remove-se as amostras sem anota????o, caso a variavel seja utilizada no treinamento, caso contr??rio manteria.")  
trainSet <- trainSet[rowSums(is.na(trainSet)) == 0,]
valSet <- valSet[rowSums(is.na(valSet)) == 0,]

correlation<-cor(trainSet) #talvez seja bom ver quais as features utilizar pela correlacao

## 2- Normalize os dados de modo que eles fiquem todos no mesmo intervalo.
##    N??o normalizaremos latitude e longitude, bem como o pre??o final
meanTrainFeatures = colMeans(trainSet[,1:8]) #mean of each feature
stdTrainFeatures = apply(trainSet[,1:8], 2, sd) #std of each feature

meanTrainFeatures
stdTrainFeatures

trainSet[,1:8] <- sweep(trainSet[,1:8], 2, meanTrainFeatures, "-")
trainSet[,1:8] = sweep(trainSet[,1:8], 2, stdTrainFeatures, "/")

valSet[,1:8] <- sweep(valSet[,1:8], 2, meanTrainFeatures, "-")
valSet[,1:8] = sweep(valSet[,1:8], 2, stdTrainFeatures, "/")

summary(trainSet)
summary(valSet)

### 2.5 - Removendo outliers
# REMOCAO DE OUTLIERS NAO SE MOSTROU EFETIVO PARA O CONJUNTO DE DADOS EM ANALISE
#trainSet = trainSet[trainSet$total_rooms < 10,]
#trainSet = trainSet[trainSet$total_bedrooms < 10,]
#trainSet = trainSet[trainSet$households < 10,]
#trainSet = trainSet[trainSet$population < 15,]
#valSet = valSet[valSet$total_rooms < 10,]
#valSet = valSet[valSet$total_bedrooms < 10,]
#valSet = valSet[valSet$households < 10,]
#valSet = valSet[valSet$population < 15,]

# graus que ser??o testados
graus <- c(1, 2, 3, 4, 5, 6, 7, 8,9,10,11)
# MAEs obtidos
MAEs_train <- c()
MAEs_test <- c()

##3 - Como baseline, fa??a uma regress??o linear para predizer os pre??os. 
##    Calcule o erro no conjunto de teste.
model_simple = lm(formula = median_house_value ~ ., data=trainSet[,1:9])
summary(model_simple)

valPred = predict(model_simple, valSet[,1:9])
summary(valPred)

MAE_simple = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_simple
MAEs_test[1] <- MAE_simple

valPred = predict(model_simple, trainSet[,1:9])
summary(valPred)

MAE_simple_train = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_simple_train
MAEs_train[1] <- MAE_simple_train

## 4 - Implemente solu????es alternativas baseadas em regress??o linear (atrav??s da 
##     combina????o dos features existentes) para melhorar os resultados obtidos no
##     baseline. Compare suas solu????es.

# Regress??o Linear simples (com os dados discretos)
formula = "median_house_value ~ ."

model_simple = lm(formula = formula, data=trainSet)
summary(model_simple)

valPred = predict(model_simple, valSet)
summary(valPred)

MAE_simple = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_simple

############# ate aqui verificado ###############
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income +
                hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                    I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(households^2) + I(median_income^2)")

# Regress??o Linear mais complexa
model_squares = lm(formula = formula_squares, data=trainSet)
summary(model_squares)

valPred = predict(model_squares, valSet)
summary(valPred)

MAE_squares = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_squares

# vamos agora filtrar os parametros menos relevantes do modelo quadrado
base_formula_filt <- "median_house_value ~ longitude + latitude +
                total_rooms + total_bedrooms + population + median_income + inland"

formula_squares_filt <- paste0(base_formula_filt, " + 
                    I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(median_income^2)")

model_squares_filt = lm(formula = formula_squares_filt, data=trainSet)
summary(model_squares_filt)

valPred = predict(model_squares_filt, valSet)
summary(valPred)

MAE_squares_filt = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_squares_filt
MAEs_test[2] <- MAE_squares_filt

valPred = predict(model_squares_filt, trainSet)
summary(valPred)

MAE_squares_filt_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_squares_filt_t
MAEs_train[2] <- MAE_squares_filt_t

# modelo c??bico
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income +
                hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                    I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(households^2) + I(median_income^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                    I(housing_median_age^3) + I(total_rooms^3) + I(total_bedrooms^3) + 
                    I(population^3) + I(households^3) + I(median_income^3)")

model_cubic = lm(formula = formula_cubic, data=trainSet)
summary(model_cubic)

valPred = predict(model_cubic, valSet)
summary(valPred)

MAE_cubic = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_cubic
MAEs_test[3] <- MAE_cubic

valPred = predict(model_cubic, trainSet)
summary(valPred)

MAE_cubic_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_cubic_t
MAEs_train[3] <- MAE_cubic_t

# modelo cubico filtrado
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income + near_ocean"

formula_squares_filt <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                          I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                          I(median_income^2)")

formula_cubic_filt <- paste0(formula_squares_filt, " + I(longitude^3) + I(latitude^3) + 
                        I(total_rooms^3) + I(total_bedrooms^3) + 
                        I(median_income^3)")

model_cubic_filt = lm(formula = formula_cubic_filt, data=trainSet)
summary(model_cubic_filt)

valPred = predict(model_cubic_filt, valSet)
summary(valPred)

MAE_cubic_filt = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_cubic_filt

# modelo de grau 4
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income +
                hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                    I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(households^2) + I(median_income^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                    I(housing_median_age^3) + I(total_rooms^3) + I(total_bedrooms^3) + 
                    I(population^3) + I(households^3) + I(median_income^3)")

formula_4 <- paste0(formula_cubic, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(total_rooms^4) + I(total_bedrooms^4) + 
                    I(population^4) + I(households^4) + I(median_income^4)")


model_4 = lm(formula = formula_4, data=trainSet)
summary(model_4)

valPred = predict(model_4, valSet)
summary(valPred)

MAE_4 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_4
MAEs_test[4] <- MAE_4

valPred = predict(model_4, trainSet)
summary(valPred)

MAE_4_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_4_t
MAEs_train[4] <- MAE_4_t

# modelo de grau 4 filtrado
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_bedrooms + population + households"

formula_squares_filt <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                          I(housing_median_age^2) + I(median_income^2)")

formula_cubic_filt <- paste0(formula_squares_filt, " + I(longitude^3) + I(latitude^3) + 
                        I(median_income^3)")

formula_4_filt <- paste0(formula_cubic_filt, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(median_income^4)")


model_4_filt = lm(formula = formula_4_filt, data=trainSet)
summary(model_4_filt)

valPred = predict(model_4_filt, valSet)
summary(valPred)

MAE_4_filt = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_4_filt

# modelo de grau 5
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
total_rooms + total_bedrooms + population + households + median_income +
hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                          I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                          I(population^2) + I(households^2) + I(median_income^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                        I(housing_median_age^3) + I(total_rooms^3) + I(total_bedrooms^3) + 
                        I(population^3) + I(households^3) + I(median_income^3)")

formula_4 <- paste0(formula_cubic, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(total_rooms^4) + I(total_bedrooms^4) + 
                    I(population^4) + I(households^4) + I(median_income^4)")

formula_5 <- paste0(formula_4, " + I(longitude^5) + I(latitude^5) + 
                    I(housing_median_age^5) + I(total_rooms^5) + I(total_bedrooms^5) + 
                    I(population^5) + I(households^5) + I(median_income^5)")


model_5 = lm(formula = formula_5, data=trainSet)
summary(model_5)

valPred = predict(model_5, valSet)
summary(valPred)

MAE_5 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_5
MAEs_test[5] <- MAE_5

valPred = predict(model_5, trainSet)
summary(valPred)

MAE_5_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_5_t
MAEs_train[5] <- MAE_5_t

# modelo de grau 6
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
total_rooms + total_bedrooms + population + households + median_income +
hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                          I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                          I(population^2) + I(households^2) + I(median_income^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                        I(housing_median_age^3) + I(total_rooms^3) + I(total_bedrooms^3) + 
                        I(population^3) + I(households^3) + I(median_income^3)")

formula_4 <- paste0(formula_cubic, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(total_rooms^4) + I(total_bedrooms^4) + 
                    I(population^4) + I(households^4) + I(median_income^4)")

formula_5 <- paste0(formula_4, " + I(longitude^5) + I(latitude^5) + 
                    I(housing_median_age^5) + I(total_rooms^5) + I(total_bedrooms^5) + 
                    I(population^5) + I(households^5) + I(median_income^5)")

formula_6 <- paste0(formula_5, " + I(longitude^6) + I(latitude^6) + 
                    I(housing_median_age^6) + I(total_rooms^6) + I(total_bedrooms^6) + 
                    I(population^6) + I(households^6) + I(median_income^6)")

model_6 = lm(formula = formula_6, data=trainSet)
summary(model_6)

valPred = predict(model_6, valSet)
summary(valPred)

MAE_6 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_6
MAEs_test[6] <- MAE_6

valPred = predict(model_6, trainSet)
summary(valPred)

MAE_6_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_6_t
MAEs_train[6] <- MAE_6_t

# modelo de grau 7
formula_7 <- paste0(formula_6, " + I(longitude^7) + I(latitude^7) + 
                    I(housing_median_age^7) + I(total_rooms^7) + I(total_bedrooms^7) + 
                    I(population^7) + I(households^7) + I(median_income^7)")


model_7 = lm(formula = formula_7, data=trainSet)
summary(model_7)

valPred = predict(model_7, valSet)
summary(valPred)

MAE_7 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_7
MAEs_test[7] <- MAE_7

valPred = predict(model_7, trainSet)
summary(valPred)

MAE_7_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_7_t
MAEs_train[7] <- MAE_7_t

# modelo de grau 8
formula_8 <- paste0(formula_7, " + I(longitude^8) + I(latitude^8) + 
                    I(housing_median_age^8) + I(total_rooms^8) + I(total_bedrooms^8) + 
                    I(population^8) + I(households^8) + I(median_income^8)")


model_8 = lm(formula = formula_8, data=trainSet)
summary(model_8)

valPred = predict(model_8, valSet)
summary(valPred)

MAE_8 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_8
MAEs_test[8] <- MAE_8

valPred = predict(model_8, trainSet)
summary(valPred)

MAE_8_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_8_t
MAEs_train[8] <- MAE_8_t

# modelo de grau 9
formula_9 <- paste0(formula_8, " + I(longitude^9) + I(latitude^9) + 
                    I(housing_median_age^9) + I(total_rooms^9) + I(total_bedrooms^9) + 
                    I(population^9) + I(households^9) + I(median_income^9)")


model_9 = lm(formula = formula_9, data=trainSet)
summary(model_9)

valPred = predict(model_9, valSet)
summary(valPred)

MAE_9 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_9

valPred = predict(model_9, trainSet)
summary(valPred)

MAE_9_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_9_t

MAEs_test[9] <- MAE_9
MAEs_train[9] <- MAE_9_t

# modelo de grau 10
formula_10 <- paste0(formula_9, " + I(longitude^10) + I(latitude^10) + 
                    I(housing_median_age^10) + I(total_rooms^10) + I(total_bedrooms^10) + 
                    I(population^10) + I(households^10) + I(median_income^10)")


model_10 = lm(formula = formula_10, data=trainSet)
summary(model_10)

valPred = predict(model_10, valSet)
summary(valPred)

MAE_10 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_10

valPred = predict(model_10, trainSet)
summary(valPred)

MAE_10_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_10_t

MAEs_train[10] <- MAE_10_t
MAEs_test[10] <- MAE_10

# modelo mais complexo
base_formula <- "median_house_value ~ housing_median_age +
population + households + median_income +
hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + 
                          I(housing_median_age^2) + 
                          I(population^2) + I(households^2) + I(median_income^2)")

formula_cubic <- paste0(formula_squares, " + 
                        I(housing_median_age^3) + 
                        I(population^3) + I(households^3) + I(median_income^3)")

formula_4 <- paste0(formula_cubic, " + 
                    I(housing_median_age^4) + 
                    I(population^4) + I(households^4) + I(median_income^4)")

formula_5 <- paste0(formula_4, " + 
                    I(housing_median_age^5) + 
                    I(population^5) + I(households^5) + I(median_income^5)")

formula_final <- paste0(formula_5, " + 
                I(sqrt(latitude^2 + longitude^2)) + I(atan2(longitude, latitude)) + 
                I(sqrt(latitude^2 + longitude^2)^2) + I(atan2(longitude, latitude)^2) +
                I(sqrt(latitude^2 + longitude^2)^3) + I(atan2(longitude, latitude)^3) +
                I(sqrt(latitude^2 + longitude^2)^4) + I(atan2(longitude, latitude)^4) +
                I(total_rooms/households) + I(total_bedrooms/households) +
                I((total_rooms/households)^2) + I((total_bedrooms/households)^2) +
                I((total_rooms/households)^3) + I((total_bedrooms/households)^3) +
                I((total_rooms/households)^4) + I((total_bedrooms/households)^4)")


model_11 = lm(formula = formula_final, data=trainSet)
summary(model_11)

valPred = predict(model_11, valSet)
summary(valPred)

MAE_11 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_11

valPred = predict(model_11, trainSet)
summary(valPred)

MAE_11_t = sum(abs(valPred - trainSet$median_house_value)) / length(valPred)
MAE_11_t

MAEs_train[11] <- MAE_11_t
MAEs_test[11] <- MAE_11

# plota grafico de MAE x grau do modelo
library(ggplot2)
type <- c(rep("train",11),rep("test",11))
graphic_data <- data.frame(Graus = rep(graus,2), MAE=c(MAEs_train,MAEs_test),Group=type)
ggplot(graphic_data,aes(x=Graus, y=MAE, group=Group,colour=Group))+geom_line()
