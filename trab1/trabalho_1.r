########################################
# Trabalho 1 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())
#setwd("~/Projects/ComplexData/inf-615/trab1")

#carrega bases
testSet<-read.csv("housePricing_testSet.csv")
trainSet<-read.csv("housePricing_trainSet.csv")
valSet<-read.csv("housePricing_valSet.csv")
summary(trainSet)
summary(valSet)
summary(testSet)

#remove nas
trainSet <- trainSet[rowSums(is.na(trainSet)) == 0,]
valSet <- valSet[rowSums(is.na(valSet)) == 0,]
testSet <- testSet[rowSums(is.na(testSet)) == 0,]
summary(trainSet)
summary(valSet)
summary(testSet)

#normaliza
meanTrainFeatures = colMeans(trainSet[,1:8]) #mean of each feature
stdTrainFeatures = apply(trainSet[,1:8], 2, sd) #std of each feature
trainSet[,1:8] <- sweep(trainSet[,1:8], 2, meanTrainFeatures, "-")
trainSet[,1:8] = sweep(trainSet[,1:8], 2, stdTrainFeatures, "/")
valSet[,1:8] <- sweep(valSet[,1:8], 2, meanTrainFeatures, "-")
valSet[,1:8] = sweep(valSet[,1:8], 2, stdTrainFeatures, "/")
testSet[,1:8] <- sweep(testSet[,1:8], 2, meanTrainFeatures, "-")
testSet[,1:8] = sweep(testSet[,1:8], 2, stdTrainFeatures, "/")
summary(trainSet)
summary(valSet)
summary(testSet)

correlation<-cor(trainSet[,1:8])

mae <- function(formula,trainSet,valSet){
  model_simple = lm(formula = formula, data=trainSet)
  #summary(model_simple)
  valPred = predict(model_simple, valSet)
  #summary(valPred)
  MAE_simple = sum(abs(valPred - valSet$median_house_value)) / length(valPred)
  return(MAE_simple)
}

#define os niveis discretos
niveis<-unique(trainSet$ocean_proximity)
levels(valSet$ocean_proximity) <- levels(trainSet$ocean_proximity)

discrete <- function(niveis,dataSet){
  dataSet$hocean = as.numeric(dataSet$ocean_proximity == niveis[1])
  dataSet$near_bay = as.numeric(dataSet$ocean_proximity == niveis[2])
  dataSet$inland = as.numeric(dataSet$ocean_proximity == niveis[3])
  dataSet$near_ocean = as.numeric(dataSet$ocean_proximity == niveis[4])
  # we only need 4 columns to represent 5 elements
  #valSet$island = as.numeric(valSet$ocean_proximity == niveis[5])
  dataSet$ocean_proximity = NULL
  return(dataSet)
}

maes_train<-c()
maes_test<-c()
maes_val<-c()

#modelo de referencia SEM dados discretos
mae("median_house_value ~ .",trainSet[,1:9],valSet[,1:9])
mae("median_house_value ~ .",trainSet[,1:9],testSet[,1:9])
mae("median_house_value ~ .",trainSet[,1:9],trainSet[,1:9])

#discretiza os dados
trainSet<-discrete(niveis,trainSet)
testSet<-discrete(niveis,testSet)
valSet<-discrete(niveis,valSet)

#modelo de referencia COM dados discretos
maes_val[1]<-mae("median_house_value ~ .",trainSet,valSet)
maes_test[1]<-mae("median_house_value ~ .",trainSet,testSet)
maes_train[1]<-mae("median_house_value ~ .",trainSet,trainSet)

#modelo com os dados menos correlacionados
base_formula<-"median_house_value ~ longitude+housing_median_age+total_rooms+total_bedrooms+median_income+hocean+inland+near_ocean"
mae(base_formula,trainSet,valSet)
mae(base_formula,trainSet,testSet)
mae(base_formula,trainSet,trainSet)

#modelo com os dados menos correlacionados I()+I()^2
base_formula<-"median_house_value ~ longitude+housing_median_age+total_rooms+total_bedrooms+median_income+hocean+inland+near_ocean+"
base_exp2<-paste0(base_formula,"I(longitude^2)+I(housing_median_age^2)+I(total_rooms^2)+I(total_bedrooms^2)+I(median_income^2)+I(hocean^2)+I(inland^2)+I(near_ocean^2)")
maes_val[2]<-mae(base_exp2,trainSet,valSet)
maes_test[2]<-mae(base_exp2,trainSet,testSet)
maes_train[2]<-mae(base_exp2,trainSet,trainSet)

#modelo com os dados menos correlacionados I()+I()^2+I()^3
base_exp3<-paste0(base_exp2,"+I(longitude^3)+I(housing_median_age^3)+I(total_rooms^3)+I(total_bedrooms^3)+I(median_income^3)+I(hocean^3)+I(inland^3)+I(near_ocean^3)")
maes_val[3]<-mae(base_exp3,trainSet,valSet)
maes_test[3]<-mae(base_exp3,trainSet,testSet)
maes_train[3]<-mae(base_exp3,trainSet,trainSet)

#modelo com os dados menos correlacionados I()+I()^2+I()^3+I()^4
base_exp4<-paste0(base_exp3,"+I(longitude^4)+I(housing_median_age^4)+I(total_rooms^4)+I(total_bedrooms^4)+I(median_income^4)+I(hocean^4)+I(inland^4)+I(near_ocean^4)")
maes_val[4]<-mae(base_exp4,trainSet,valSet)
maes_test[4]<-mae(base_exp4,trainSet,testSet)
maes_train[4]<-mae(base_exp4,trainSet,trainSet)

#modelo com os dados menos correlacionados I()+I()^2+I()^3+I()^4+I()^5
base_exp5<-paste0(base_exp4,"+I(longitude^5)+I(housing_median_age^5)+I(total_rooms^5)+I(total_bedrooms^5)+I(median_income^5)+I(hocean^5)+I(inland^5)+I(near_ocean^5)")
maes_val[5]<-mae(base_exp5,trainSet,valSet)
maes_test[5]<-mae(base_exp5,trainSet,testSet)
maes_train[5]<-mae(base_exp5,trainSet,trainSet)

#modelo com os dados menos correlacionados I()+I()^2+I()^3+I()^4+I()^5+I()^6
base_exp6<-paste0(base_exp5,"+I(longitude^6)+I(housing_median_age^6)+I(total_rooms^6)+I(total_bedrooms^6)+I(median_income^6)+I(hocean^6)+I(inland^6)+I(near_ocean^6)")
maes_val[6]<-mae(base_exp6,trainSet,valSet)
maes_test[6]<-mae(base_exp6,trainSet,testSet)
maes_train[6]<-mae(base_exp6,trainSet,trainSet)

#modelo com os dados menos correlacionados I()+I()^2+I()^3+I()^4+I()^5+I()^6+I()^7
base_exp7<-paste0(base_exp6,"+I(longitude^7)+I(housing_median_age^7)+I(total_rooms^7)+I(total_bedrooms^7)+I(median_income^7)+I(hocean^7)+I(inland^7)+I(near_ocean^7)")
maes_val[7]<-mae(base_exp7,trainSet,valSet)
maes_test[7]<-mae(base_exp7,trainSet,testSet)
maes_train[7]<-mae(base_exp7,trainSet,trainSet)

# modelo mais complexo
base_formula <- "median_house_value ~ longitude + latitude + housing_median_age +
total_rooms + total_bedrooms + population + households + median_income +
hocean + near_bay + inland + near_ocean"

formula_squares <- paste0(base_formula, " + I(latitude^2) + 
                          I(housing_median_age^2) + I(total_bedrooms^2) + 
                          I(population^2) + I(median_income^2)")

formula_cubic <- paste0(formula_squares, " + I(longitude^3) + I(latitude^3) + 
                        I(housing_median_age^3) + I(total_bedrooms^3) + 
                        I(population^3) + I(households^3) + I(median_income^3)")

formula_4 <- paste0(formula_cubic, " + I(longitude^4) + I(latitude^4) + 
                    I(housing_median_age^4) + I(total_bedrooms^4) + 
                    I(population^4) + I(households^4) + I(median_income^4)")

formula_5 <- paste0(formula_4, " + I(longitude^5) + I(latitude^5) + 
                    I(housing_median_age^5) + I(total_bedrooms^5) + 
                    I(population^5) + I(households^5) + I(median_income^5)")

formula_final <- paste0(formula_5, " + 
                        I(longitude*hocean) + I(longitude*near_bay) + I(longitude*near_ocean) +
                        I((longitude*hocean)^2) + I((longitude*near_bay)^2) + I((longitude*near_ocean)^2) +
                        I((longitude*hocean)^3) + I((longitude*near_bay)^3) + I((longitude*near_ocean)^3)")

mae(formula_final,trainSet,valSet)
mae(formula_final,trainSet,testSet)
mae(formula_final,trainSet,trainSet)

# plota grafico de MAE x grau do modelo
library(ggplot2)
type <- c(rep("treino",7),rep("teste",7),rep("validacao",7))
graphic_data <- data.frame(Graus = rep(c(1, 2, 3, 4, 5,6,7),3), MAE=c(maes_train,maes_test,maes_val),Group=type)
ggplot(graphic_data,aes(x=Graus, y=MAE, group=Group,colour=Group))+geom_line()
