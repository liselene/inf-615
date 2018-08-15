########################################
# Trabalho 2 - INF-615
# Nome(s): Liselene Borges e
########################################
rm(list=ls())
#setwd("~/Projects/ComplexData/inf-615/trab2")

ACC <- function(cm) {
  #ACC = (TP + TN) / total
  ACC = (cm[1,1] + cm[2,2]) / sum(cm)
  return(ACC)
}

ACCNorm <- function(cm) {
  #TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
  TPR = cm[2,2] / (cm[2,2] + cm[2,1])
  TPR
  #TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
  TNR = cm[1,1] / (cm[1,1] + cm[1,2])
  TNR
  #ACC normalized by class
  #takes into account the number of samples for each class
  ACCNorm_glm = mean(c(TPR, TNR))
  return(ACCNorm_glm)
}

## carregando os dados
train<-read.csv("wineQuality_train.data")
val<-read.csv("wineQuality_val.data")

## inpecionando os dados
summary(train)
summary(val)
nrow(train)
nrow(val)

boxplot(train[,-12], 
        main="Range dos dados", 
        xlab="Tipo", ylab="Grandeza")

# plot apenas dos vinhos ruins
boxplot(train[train$quality == 0, c(1, 2, 3, 4, 5, 8, 9, 10, 11)], 
        main="Range dos dados", 
        xlab="Tipo", ylab="Grandeza", ylim = c(0,30))

# plot apenas dos vinhos bons
boxplot(train[train$quality == 1, c(1, 2, 3, 4, 5, 8, 9, 10, 11)], 
        main="Range dos dados", 
        xlab="Tipo", ylab="Grandeza", ylim = c(0,30))

## Qual o intervalo de valores de cada feature?
sapply(train, max)
sapply(train, min)

## Há aproximadamente a mesma quantidade de exemplos de cada classe? Ou seja, as classes estão balanceadas?
#As classes são desbalanceadas, tem 4x mais amotras da classe 0 do que da 1: 771(1) e 3127(0)
sum(train$quality==1)
sum(train$quality==0)

## Como você lidaria com classes desbalanceadas?
# As métricas de avaliação do classificador levarão em consideração a proporção de amostras de cada classe
# utilizando, por exemplo, a matriz de confusão. Calculando também precisão(total depositivos corretos/(total de positivos corretos + incorretos)
# e o recall (total de negativos corretos/(total de negativo corretos + incorretos))

## Normalize os dados de modo que eles fiquem todos no mesmo intervalo.
train_mean <- colMeans(train[,1:11]) #mean of each feature
train_sd  <- apply(train[,1:11], 2, sd) #std of each feature

train[,1:11] = sweep(train[,1:11], 2, train_mean, "-")
train[,1:11] = sweep(train[,1:11], 2, train_sd, "/")

val[,1:11] = sweep(val[,1:11], 2, train_mean, "-")
val[,1:11] = sweep(val[,1:11], 2, train_sd, "/")

## Treine uma regressão logística com todas as features para predizer a qualidade dos vinhos (baseline).
formula = as.formula("quality ~ .")
logRegModel = glm(formula, train, family=binomial(link="logit"))
summary(logRegModel)

# probabilities of being the class 0
valPred = predict(logRegModel, val[,-12], type="response")
#valPred

#converting to class
valPred[valPred >= 0.5] = 1
valPred[valPred < 0.5] = 0

cm <- as.matrix(table(Actual = val$quality, Predicted = valPred))
cm

ACC(cm)
ACCNorm(cm)


## Implemente soluções alternativas baseadas em regressão logística (através da combinação dos features exis-
# tentes) para melhorar os resultados obtidos no baseline.
cor(train[,-12])
regra <- "quality ~ fixed.acidity+volatile.acidity+citric.acid+
          residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates"
formula = as.formula(regra)
logRegModel = glm(formula, train, family=binomial(link="logit"))
summary(logRegModel)

# probabilities of being the class 0
valPred = predict(logRegModel, val[,-12], type="response")
valPred

#converting to class
valPred[valPred >= 0.5] = 1
valPred[valPred < 0.5] = 0

cm <- as.matrix(table(Actual = val$quality, Predicted = valPred))
cm

ACC(cm)
ACCNorm(cm)

########################################################################################
# Marcos - Vou criar 4 modelos diferentes, separando os dados de treino em 4 partes
########################################################################################
bad_wines_df = train[train$quality == 0, ]
good_wines_df = train[train$quality == 1, ]

bad_dfs <- split(bad_wines_df, rep(1:4, each=nrow(bad_wines_df)/4, length.out=nrow(bad_wines_df)))

dfs <- list()
dfs[[1]] <- rbind(good_wines_df, bad_dfs[[1]])
dfs[[2]] <- rbind(good_wines_df, bad_dfs[[2]])
dfs[[3]] <- rbind(good_wines_df, bad_dfs[[3]])
dfs[[4]] <- rbind(good_wines_df, bad_dfs[[4]])

regra <- "quality ~ fixed.acidity+volatile.acidity+citric.acid+
          residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates"
formula = as.formula(regra)

#array of zeros to sum all valPred's
final_Pred <- rep(0, nrow(val))
for (i in 1:4) {
  logRegModel = glm(formula, dfs[[i]], family=binomial(link="logit"))
  valPred = predict(logRegModel, val[,-12], type="response")
  final_Pred <- final_Pred + valPred
}

final_Pred[final_Pred < 1.5] = 0
final_Pred[final_Pred >= 1.5] = 1

cm <- as.matrix(table(Actual = val$quality, Predicted = final_Pred))
print(cm)

ACC(cm)
ACCNorm(cm)

########################################################################################
# Marcos - Mesma coisa com glmnet
########################################################################################
library("glmnet")

#array of zeros to sum all valPred's
final_Pred <- rep(0, nrow(val))
for (i in 1:4) {
  x = model.matrix(quality~.+0, dfs[[i]])
  y = dfs[[i]]$quality
  
  model = glmnet(x, y,  family="binomial", alpha=0, lambda = 0.0001)
  coef(model)
  
  x_test = model.matrix(quality~.+0, val)
  testPred = predict(model,newx = x_test, type="response")
  final_Pred <- final_Pred + testPred
}

final_Pred[final_Pred < 1.3] = 0
final_Pred[final_Pred >= 1.3] = 1

cm <- as.matrix(table(Actual = val$quality, Predicted = final_Pred))
print(cm)

ACC(cm)
ACCNorm(cm)

########################################################################################
# Marcos - glmnet GRAU 2
########################################################################################

regra <- quality ~ fixed.acidity+volatile.acidity+citric.acid+
  residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
  density+pH+sulphates+alcohol+
  I(fixed.acidity^2)+I(volatile.acidity^2)+I(citric.acid^2)+
  I(residual.sugar^2)+I(chlorides^2)+I(free.sulfur.dioxide^2)+I(total.sulfur.dioxide^2)+
  I(density^2)+I(pH^2)+I(sulphates^2)+I(alcohol^2)+0

#array of zeros to sum all valPred's
final_Pred <- rep(0, nrow(val))
for (i in 1:4) {
  x = model.matrix(regra, dfs[[i]])
  y = dfs[[i]]$quality
  
  model = glmnet(x, y,  family="binomial", alpha=0, lambda = 0.00005)
  coef(model)
  
  x_test = model.matrix(regra, val)
  testPred = predict(model,newx = x_test, type="response")
  final_Pred <- final_Pred + testPred
}

final_Pred[final_Pred < 1.45] = 0
final_Pred[final_Pred >= 1.45] = 1

cm <- as.matrix(table(Actual = val$quality, Predicted = final_Pred))
print(cm)

ACC(cm)
ACCNorm(cm)

########################################################################################
# Marcos - glmnet GRAU 3
########################################################################################

regra <- quality ~ fixed.acidity+volatile.acidity+citric.acid+
  residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
  density+pH+sulphates+alcohol+
  I(fixed.acidity^2)+I(volatile.acidity^2)+I(citric.acid^2)+
  I(residual.sugar^2)+I(chlorides^2)+I(free.sulfur.dioxide^2)+I(total.sulfur.dioxide^2)+
  I(density^2)+I(pH^2)+I(sulphates^2)+I(alcohol^2)+
  I(fixed.acidity^3)+I(volatile.acidity^3)+I(citric.acid^3)+
  I(residual.sugar^3)+I(chlorides^3)+I(free.sulfur.dioxide^3)+I(total.sulfur.dioxide^3)+
  I(density^3)+I(pH^3)+I(sulphates^3)+I(alcohol^3)+0

#array of zeros to sum all valPred's
final_Pred <- rep(0, nrow(val))
for (i in 1:4) {
  x = model.matrix(regra, dfs[[i]])
  y = dfs[[i]]$quality
  
  model = glmnet(x, y,  family="binomial", alpha=0, lambda = 0.00003)
  coef(model)
  
  x_test = model.matrix(regra, val)
  testPred = predict(model,newx = x_test, type="response")
  final_Pred <- final_Pred + testPred
}

final_Pred[final_Pred < 1.1] = 0
final_Pred[final_Pred >= 1.1] = 1

cm <- as.matrix(table(Actual = val$quality, Predicted = final_Pred))
print(cm)

ACC(cm)
ACCNorm(cm)

########################################################################################
# Marcos - glmnet GRAU 4
########################################################################################

regra <- quality ~ fixed.acidity+volatile.acidity+citric.acid+
  residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
  density+pH+sulphates+alcohol+
  I(fixed.acidity^2)+I(volatile.acidity^2)+I(citric.acid^2)+
  I(residual.sugar^2)+I(chlorides^2)+I(free.sulfur.dioxide^2)+I(total.sulfur.dioxide^2)+
  I(density^2)+I(pH^2)+I(sulphates^2)+I(alcohol^2)+
  I(fixed.acidity^3)+I(volatile.acidity^3)+I(citric.acid^3)+
  I(residual.sugar^3)+I(chlorides^3)+I(free.sulfur.dioxide^3)+I(total.sulfur.dioxide^3)+
  I(density^3)+I(pH^3)+I(sulphates^3)+I(alcohol^3)+
  I(fixed.acidity^4)+I(volatile.acidity^4)+I(citric.acid^4)+
  I(residual.sugar^4)+I(chlorides^4)+I(free.sulfur.dioxide^4)+I(total.sulfur.dioxide^4)+
  I(density^4)+I(pH^4)+I(sulphates^4)+I(alcohol^4)+0

#array of zeros to sum all valPred's
final_Pred <- rep(0, nrow(val))
for (i in 1:4) {
  x = model.matrix(regra, dfs[[i]])
  y = dfs[[i]]$quality
  
  model = glmnet(x, y,  family="binomial", alpha=0, lambda = 0.0001)
  coef(model)
  
  x_test = model.matrix(regra, val)
  testPred = predict(model,newx = x_test, type="response")
  final_Pred <- final_Pred + testPred
}

final_Pred[final_Pred < 1.2] = 0
final_Pred[final_Pred >= 1.2] = 1

cm <- as.matrix(table(Actual = val$quality, Predicted = final_Pred))
print(cm)

ACC(cm)
ACCNorm(cm)
