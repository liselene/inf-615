########################################
# Trabalho 2 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())
#setwd("~/Documents/Curso - Complex Data/INF-0615/Tarefa1/inf-615/trab2")
setwd("~/Projects/ComplexData/inf-615/trab2")

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
        main="Quality = 0", 
        xlab="Tipo", ylab="Grandeza", ylim = c(0,30))

# plot apenas dos vinhos bons
boxplot(train[train$quality == 1, c(1, 2, 3, 4, 5, 8, 9, 10, 11)], 
        main="Quality = 1", 
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
valPred = predict(logRegModel, val, type="response")
valPred

#converting to class
valPred[valPred >= 0.5] = 1
valPred[valPred < 0.5] = 0

cm <- as.matrix(table(Actual = val$quality, Predicted = valPred))
cm

ACC(cm)
ACCNorm(cm)


## Implemente soluções alternativas baseadas em regressão logística (através da combinação dos features exis-
# tentes) para melhorar os resultados obtidos no baseline.
#
# Criar 4 modelos diferentes, separando os dados de treino em 4 partes
##
bad_wines_df = train[train$quality == 0, ]
good_wines_df = train[train$quality == 1, ]

bad_dfs <- split(bad_wines_df, rep(1:4, each=nrow(bad_wines_df)/4, length.out=nrow(bad_wines_df)))

dfs <- list()
dfs[[1]] <- rbind(good_wines_df, bad_dfs[[1]])
dfs[[2]] <- rbind(good_wines_df, bad_dfs[[2]])
dfs[[3]] <- rbind(good_wines_df, bad_dfs[[3]])
dfs[[4]] <- rbind(good_wines_df, bad_dfs[[4]])

regra <- "quality ~ ."#fixed.acidity+volatile.acidity+citric.acid+
          #residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates"
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
# Mesma coisa com glmnet
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
# Aplicando o método de pasting, criando diversos dataframes com dados duplicados
########################################################################################
calc_regr_pasting <- function(train_dfs, regra, test_df) {
  final_Pred <- rep(0, nrow(test_df))
  for (i in 1:length(train_dfs)) {
    x = model.matrix(regra, train_dfs[[i]])
    y = train_dfs[[i]]$quality
    
    model = glmnet(x, y,  family="binomial", alpha=0, lambda = 0.00001)
    #coef(model)
    
    x_test = model.matrix(regra, test_df)
    testPred = predict(model,newx = x_test, type="response")
    testPred[testPred < 0.15] = 0 #VARIAR - add or remove
    testPred[testPred >= 0.3] = 1 #VARIAR - add or remove
    
    final_Pred <- final_Pred + testPred
  }
  final_Pred[final_Pred <= length(train_dfs)/3.8] = 0 # VARIAR - divisor of length(train_dfs)
  final_Pred[final_Pred > length(train_dfs)/3.8] = 1 # VARIAR - divisor of length(train_dfs)
  
  cm <- as.matrix(table(Actual = val$quality, Predicted = final_Pred))
  print(cm)
  return(cm)
}

bad_wines_df = train[train$quality == 0, ]
good_wines_df = train[train$quality == 1, ]

nrow(bad_wines_df)
nrow(good_wines_df)

regra <- quality ~ fixed.acidity+volatile.acidity+citric.acid+
  residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates+alcohol+
  I(fixed.acidity^2)+I(volatile.acidity^2)+I(citric.acid^2)+
  I(residual.sugar^2)+I(chlorides^2)+I(free.sulfur.dioxide^2)+
  I(pH^2)+I(sulphates^2)+
  I(fixed.acidity^3)+I(volatile.acidity^3)+I(citric.acid^3)+
  I(residual.sugar^3)+I(chlorides^3)+I(free.sulfur.dioxide^3)+
  I(pH^3)+I(sulphates^3)+
  I(fixed.acidity*volatile.acidity+citric.acid)+
  I((fixed.acidity+volatile.acidity+citric.acid)^2)+
  I((fixed.acidity+volatile.acidity+citric.acid)^3)+
  I((free.sulfur.dioxide+total.sulfur.dioxide))+
  I((free.sulfur.dioxide+total.sulfur.dioxide)^2)+
  I((free.sulfur.dioxide+total.sulfur.dioxide)^3)+
  I(residual.sugar*alcohol)+
  I((residual.sugar*alcohol)^2)+
  I((residual.sugar*alcohol)^3)+
  I(fixed.acidity*volatile.acidity*citric.acid*
      residual.sugar*chlorides*free.sulfur.dioxide*pH*sulphates*alcohol)+
  I((fixed.acidity*volatile.acidity*citric.acid*
      residual.sugar*chlorides*free.sulfur.dioxide*pH*sulphates*alcohol)^2)+
  I((fixed.acidity*volatile.acidity*citric.acid*
       residual.sugar*chlorides*free.sulfur.dioxide*pH*sulphates*alcohol)^3)+
  I(fixed.acidity+volatile.acidity+citric.acid+
      residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates+alcohol)+
  I((fixed.acidity+volatile.acidity+citric.acid+
       residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates+alcohol)^2)+
  I((fixed.acidity+volatile.acidity+citric.acid+
       residual.sugar+chlorides+free.sulfur.dioxide+pH+sulphates+alcohol)^3)+0

train_dfs <- list()

all_ACCs = c()
all_ACCsNorm = c()
# 1 dataset
train_dfs[[1]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                  good_wines_df[sample(nrow(good_wines_df), 770),])
cm <- calc_regr_pasting(train_dfs, regra, val)
all_ACCs <- c(all_ACCs, ACC(cm))
all_ACCsNorm <- c(all_ACCsNorm, ACCNorm(cm))


# 3 datasets
train_dfs[[2]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
train_dfs[[3]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
cm <- calc_regr_pasting(train_dfs, regra, val)
all_ACCs <- c(all_ACCs, ACC(cm))
all_ACCsNorm <- c(all_ACCsNorm, ACCNorm(cm))

# 5 datasets
train_dfs[[4]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
train_dfs[[5]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
cm <- calc_regr_pasting(train_dfs, regra, val)
all_ACCs <- c(all_ACCs, ACC(cm))
all_ACCsNorm <- c(all_ACCsNorm, ACCNorm(cm))

# 7 datasets
train_dfs[[6]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
train_dfs[[7]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])

cm <- calc_regr_pasting(train_dfs, regra, val)
all_ACCs <- c(all_ACCs, ACC(cm))
all_ACCsNorm <- c(all_ACCsNorm, ACCNorm(cm))

# 9 datasets
train_dfs[[8]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
train_dfs[[9]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
cm <- calc_regr_pasting(train_dfs, regra, val)
all_ACCs <- c(all_ACCs, ACC(cm))
all_ACCsNorm <- c(all_ACCsNorm, ACCNorm(cm))

# 11 datasets
train_dfs[[10]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
train_dfs[[11]] <- rbind(bad_wines_df[sample(nrow(bad_wines_df), 770),], 
                        good_wines_df[sample(nrow(good_wines_df), 770),])
cm <- calc_regr_pasting(train_dfs, regra, val)
all_ACCs <- c(all_ACCs, ACC(cm))
all_ACCsNorm <- c(all_ACCsNorm, ACCNorm(cm))

nModels <- c(1, 3, 5, 7, 9, 11)
print(all_ACCs)
print(all_ACCsNorm)

# plot graphic for different datasets using pasting
library(ggplot2)
type <- c(rep("ACCs",6),rep("ACCsNorm",6))
graphic_data <- data.frame(Dataset = rep(nModels,2), Accuracy=c(all_ACCs,all_ACCsNorm),Group=type)
ggplot(graphic_data,aes(x=Dataset, y=Accuracy, group=Group,colour=Group))+geom_line()
