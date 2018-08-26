########################################
# Trabalho 3 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())
#setwd("~/Projects/ComplexData/inf-615/trab3")
setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab3")

set.seed(42)

predictAndEvaluateTree <- function(model, data){
  prediction = predict(model, data)
  prediction = as.numeric(prediction[,2] >= 0.5)
  
  CM = as.matrix(table(Actual = data$approved, Predicted = prediction))
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm = mean(c(TPR, TNR))
  
  return(list(CM=CM, ACCNorm=ACCNorm))
}

predictAndEvaluateForest <- function(model, data){
  prediction = predict(model, data) 
  CM = as.matrix(table(Actual = data$approved, Predicted = prediction))
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm = mean(c(TPR, TNR))
  return(list(CM=CM, ACCNorm=ACCNorm))
}

## carregando os dados
train<-read.csv("student_performance_train.data")
val<-read.csv("student_performance_val.data")
test<-read.csv("student_performance_test.data")

## inpecionando os dados
summary(train)
summary(val)
nrow(train) #626
nrow(val) #209
nrow(test) #209

## intervalo das features
#sapply(train, max)
#sapply(train, min)

train$approved = as.factor(train$approved)
val$approved = as.factor(val$approved)
test$approved = as.factor(test$approved)

## quantidade de exemplos de cada classe
sum(train$approved==1)
sum(train$approved==0)

## decision tree
library(rpart)

##ACC Vs Depth 
accPerDepth = data.frame(depth=numeric(30), accTrain=numeric(30), accVal=numeric(30), accTest=numeric(30))
for (maxDepth in 3:30){
  treeModel = rpart(formula=approved~ ., 
                    data=train, method="class",
                    control=rpart.control(minsplit=30, cp=0.008, maxdepth=maxDepth),
                    parms= list(split="information"))
  
  trainResults = predictAndEvaluateTree(treeModel, train)
  valResults = predictAndEvaluateTree(treeModel, val)
  testResults = predictAndEvaluateTree(treeModel, test)
  
  accPerDepth[maxDepth,] = c(maxDepth, trainResults$ACCNorm, valResults$ACCNorm, testResults$ACCNorm)
}

## Plot
library("reshape2")
library("ggplot2")
accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line()

treeModel = rpart(formula=approved ~ ., 
                  data=train, method="class",
                  control=rpart.control(minsplit=30, cp=0.008, maxdepth=10),
                  parms= list(split="information"))



##train
trainResults = predictAndEvaluateTree(treeModel, train)
trainResults$CM
trainResults$ACCNorm
#val
valResults = predictAndEvaluateTree(treeModel, val)
valResults$CM
valResults$ACCNorm
#test
testResults = predictAndEvaluateTree(treeModel, test)
testResults$CM
testResults$ACCNorm

##Plot DT
plot(treeModel, uniform=TRUE)
text(treeModel, use.n=TRUE, all=TRUE, cex=.8)

## Random Forest
library(randomForest)
library(DMwR)

#tr <- trainControl(method = "cv", number = 20)

nTree = c(5,10,25,50,75,100,125,150,175,200)
accPerNTree = data.frame(ntree=numeric(5), accTrain=numeric(5), accVal=numeric(5),accTest=numeric(5))
for (i in 1:10){
  rfModel = randomForest(formula=approved~ ., data= train, ntree=nTree[i],
                         nodesize=13, maxnodes=150, mtry=8)
  rfACCNormTrain = predictAndEvaluateForest(rfModel, train)
  rfACCNormVal = predictAndEvaluateForest(rfModel, val)
  rfACCNormTest = predictAndEvaluateForest(rfModel, test)
  accPerNTree[i,] = c(nTree[i], rfACCNormTrain$ACCNorm, rfACCNormVal$ACCNorm,rfACCNormTest$ACCNorm)
}

library(ggplot2)
type <- c(rep("accTrain",10),rep("accVal",10),rep("accTest",10))
graphic_data <- data.frame(ntree=rep(accPerNTree$ntree,3),
                           Dataset=c(accPerNTree$accTrain,accPerNTree$accVal,accPerNTree$accTest),
                           Group=type)
ggplot(data=graphic_data, aes(x=ntree, y=Dataset, colour=Group))+geom_line()


# applying Bagging
newData <- SMOTE(approved ~ ., train, perc.over = 1000,perc.under=117)
new_approved <- newData[newData$approved == 1,]
new_not_approved <- newData[newData$approved == 0,]
nrow(new_approved)
nrow(new_not_approved)

prediction_train = c()
prediction_val = c()
prediction_test = c()
for (i in 1:11) {
  train_data <- rbind(new_approved[(1+(i-1)*306):(i*306),],
                      new_not_approved[(1+(i-1)*320):(i*320),])
  rfModel = randomForest(formula=approved~ ., data= train_data, ntree=500,
                         nodesize=13, maxnodes=150, mtry=8)
  
  prediction_train <- cbind(prediction_train, predict(rfModel, train))
  prediction_val <- cbind(prediction_val, predict(rfModel, val))
  prediction_test <- cbind(prediction_test, predict(rfModel, test))
}

prediction_train <- as.numeric(prediction_train > 1)
prediction_train <- matrix(prediction_train, ncol=11)
prediction_val <- as.numeric(prediction_val > 1)
prediction_val <- matrix(prediction_val, ncol=11)
prediction_test <- as.numeric(prediction_test > 1)
prediction_test <- matrix(prediction_test, ncol=11)

prediction_train <- as.numeric(rowSums(prediction_train) > 0)
prediction_val <- as.numeric(rowSums(prediction_val) > 0)
prediction_test <- as.numeric(rowSums(prediction_test) > 0)

CM = as.matrix(table(Actual = train$approved, Predicted = prediction_train))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm_train = mean(c(TPR, TNR))
CM = as.matrix(table(Actual = val$approved, Predicted = prediction_val))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm_val = mean(c(TPR, TNR))
CM = as.matrix(table(Actual = test$approved, Predicted = prediction_test))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm_test = mean(c(TPR, TNR))
ACCNorm_train
ACCNorm_val
ACCNorm_test


# other bagging
library(ipred)


model = ipredbagg(train[,"approved"], X=train[,1:30], nbagg=150)

baggPrediction = predict(model, val) 
baggCM = as.matrix(table(Actual = val$approved, Predicted = baggPrediction))
baggTPR = baggCM[2,2] / (baggCM[2,2] + baggCM[2,1])
baggTNR = baggCM[1,1] / (baggCM[1,1] + baggCM[1,2])
baggACCNormVal = mean(c(baggTPR, baggTNR))
baggACCNormVal

baggPrediction = predict(model, test) 
baggCM = as.matrix(table(Actual = test$approved, Predicted = baggPrediction))
baggTPR = baggCM[2,2] / (baggCM[2,2] + baggCM[2,1])
baggTNR = baggCM[1,1] / (baggCM[1,1] + baggCM[1,2])
baggACCNormTest = mean(c(baggTPR, baggTNR))
baggACCNormTest
