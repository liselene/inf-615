########################################
# Trabalho 2 - INF-615
# Nome(s): Liselene Borges e
########################################
rm(list=ls())
setwd("~/Projects/ComplexData/inf-615/trab2")
library("glmnet")

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
train_mean <- colMeans(train[,-12]) #mean of each feature
train_sd  <- apply(train[,-12], 2, sd) #std of each feature

train[,-12] = sweep(train[,-12], 2, train_mean, "-")
train[,-12] = sweep(train[,-12], 2, train_sd, "/")

val[,-12] = sweep(val[,-12], 2, train_mean, "-")
val[,-12] = sweep(val[,-12], 2, train_sd, "/")

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

#ACC = (TP + TN) / total
ACC = (cm[1,1] + cm[2,2]) / sum(cm)
ACC

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR = cm[2,2] / (cm[2,2] + cm[2,1])
TPR

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR = cm[1,1] / (cm[1,1] + cm[1,2])
TNR

#ACC normalized by class
#takes into account the number of samples for each class
ACCNorm_glm = mean(c(TPR, TNR))
ACCNorm_glm


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

#ACC = (TP + TN) / total
ACC = (cm[1,1] + cm[2,2]) / sum(cm)
ACC

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR = cm[2,2] / (cm[2,2] + cm[2,1])
TPR

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR = cm[1,1] / (cm[1,1] + cm[1,2])
TNR

#ACC normalized by class
#takes into account the number of samples for each class
ACCNorm_glm = mean(c(TPR, TNR))
ACCNorm_glm

