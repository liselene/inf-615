rm(list=ls())

valSet<-read.csv("housePricing_valSet.csv")
trainSet<-read.csv("housePricing_trainSet.csv")

summary(trainSet)
summary(valSet)

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
#trainSet$island = as.numeric(trainSet$ocean_proximity == niveis[5])
trainSet$ocean_proximity = NULL

# TODO: precisa arrumar o factor
valSet$hocean = as.numeric(valSet$ocean_proximity == niveis[1])
valSet$near_bay = as.numeric(valSet$ocean_proximity == niveis[2])
valSet$inland = as.numeric(valSet$ocean_proximity == niveis[3])
valSet$near_ocean = as.numeric(valSet$ocean_proximity == niveis[4])
# we only need 4 columns to represent 5 elements
#valSet$island = as.numeric(valSet$ocean_proximity == niveis[5])
valSet$ocean_proximity = NULL

# Há exemplos com features sem anotações? 
paste("Sim")

# Como você lidaria com isso?
paste("Remove-se as amostras sem anotação, caso a variavel seja utilizada no treinamento, caso contrário manteria.")  
trainSet <- trainSet[rowSums(is.na(trainSet)) == 0,]
valSet <- valSet[rowSums(is.na(valSet)) == 0,]

correlation<-cor(trainSet[,1:8]) #talvez seja bom ver quais as features utilizar pela correlacao

## 2- Normalize os dados de modo que eles fiquem todos no mesmo intervalo.
##    Não normalizaremos latitude e longitude, bem como o preço final
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
# REMOÇÃO DE OUTLIERS NÃO SE MOSTROU EFETIVO PARA O CONJUNTO DE DADOS EM ANÁLISE
#trainSet = trainSet[trainSet$total_rooms < 10,]
#trainSet = trainSet[trainSet$total_bedrooms < 10,]
#trainSet = trainSet[trainSet$households < 10,]
#trainSet = trainSet[trainSet$population < 15,]
#valSet = valSet[valSet$total_rooms < 10,]
#valSet = valSet[valSet$total_bedrooms < 10,]
#valSet = valSet[valSet$households < 10,]
#valSet = valSet[valSet$population < 15,]


##3 - Como baseline, faça uma regressão linear para predizer os preços. 
##    Calcule o erro no conjunto de teste.
model_simple = lm(formula = median_house_value ~ ., data=trainSet[,1:9])
summary(model_simple)

valPred = predict(model_simple, valSet[,1:9])
summary(valPred)

MAE_simple = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_simple

## 4 - Implemente soluções alternativas baseadas em regressão linear (através da 
##     combinação dos features existentes) para melhorar os resultados obtidos no
##     baseline. Compare suas soluções.

# Regressão Linear simples (com os dados discretos)
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
                median_house_value + hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                    I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(households^2) + I(median_income^2) +
                    I(median_house_value^2)")

# Regressão Linear mais complexa
model_squares = lm(formula = formula_squares, data=trainSet)
summary(model_squares)

valPred = predict(model_squares, valSet)
summary(valPred)

MAE_squares = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_squares

# vamos agora filtrar os parametros menos relevantes do modelo quadrado
base_formula_filt <- "median_house_value ~ longitude + latitude +
                total_rooms + total_bedrooms + population + median_income +
                median_house_value + inland"

formula_squares_filt <- paste0(base_formula_filt, " + 
                    I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(median_income^2) +
                    I(median_house_value^2)")

model_squares_filt = lm(formula = formula_squares_filt, data=trainSet)
summary(model_squares_filt)

valPred = predict(model_squares_filt, valSet)
summary(valPred)

MAE_squares_filt = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_squares_filt

# modelo cúbico
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income +
                median_house_value + hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                    I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(households^2) + I(median_income^2) +
                    I(median_house_value^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                    I(housing_median_age^3) + I(total_rooms^3) + I(total_bedrooms^3) + 
                    I(population^3) + I(households^3) + I(median_income^3) +
                    I(median_house_value^3)")

model_cubic = lm(formula = formula_cubic, data=trainSet)
summary(model_cubic)

valPred = predict(model_cubic, valSet)
summary(valPred)

MAE_cubic = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_cubic

# modelo cúbico filtrado
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income +
                median_house_value + near_ocean"

formula_squares_filt <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                          I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                          I(median_income^2) +
                          I(median_house_value^2)")

formula_cubic_filt <- paste0(formula_squares_filt, " + I(longitude^3) + I(latitude^3) + 
                        I(total_rooms^3) + I(total_bedrooms^3) + 
                        I(median_income^3) +
                        I(median_house_value^3)")

model_cubic_filt = lm(formula = formula_cubic_filt, data=trainSet)
summary(model_cubic_filt)

valPred = predict(model_cubic_filt, valSet)
summary(valPred)

MAE_cubic_filt = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_cubic_filt

# modelo de grau 4
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_rooms + total_bedrooms + population + households + median_income +
                median_house_value + hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                    I(housing_median_age^2) + I(total_rooms^2) + I(total_bedrooms^2) + 
                    I(population^2) + I(households^2) + I(median_income^2) +
                    I(median_house_value^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                    I(housing_median_age^3) + I(total_rooms^3) + I(total_bedrooms^3) + 
                    I(population^3) + I(households^3) + I(median_income^3) +
                    I(median_house_value^3)")

formula_4 <- paste0(formula_cubic, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(total_rooms^4) + I(total_bedrooms^4) + 
                    I(population^4) + I(households^4) + I(median_income^4) +
                    I(median_house_value^4)")


model_4 = lm(formula = formula_4, data=trainSet)
summary(model_4)

valPred = predict(model_4, valSet)
summary(valPred)

MAE_4 = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_4

# modelo de grau 4 filtrado
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
                total_bedrooms + population + households +
                median_house_value"

formula_squares_filt <- paste0(base_formula, " + I(longitude^2) + I(latitude^2) + 
                          I(housing_median_age^2) + I(median_income^2) +
                          I(median_house_value^2)")

formula_cubic_filt <- paste0(formula_squares_filt, " + I(longitude^3) + I(latitude^3) + 
                        I(median_income^3) +
                        I(median_house_value^3)")

formula_4_filt <- paste0(formula_cubic_filt, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(median_income^4) +
                    I(median_house_value^4)")


model_4_filt = lm(formula = formula_4_filt, data=trainSet)
summary(model_4_filt)

valPred = predict(model_4_filt, valSet)
summary(valPred)

MAE_4_filt = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_4_filt

# Regressão Linear mais complexa
model_complex = lm(formula = median_house_value ~ I(total_rooms^2)+I(households^2) #
                   , data=trainSet)
summary(model_complex)

valPred = predict(model_complex, valSet)
summary(valPred)

MAE_complex = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
MAE_complex

