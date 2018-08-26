########################################
# Trabalho 3 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())
setwd("~/Projects/ComplexData/inf-615/trab3")
#setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab3")

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

evaluateForestBagging <- function(data,prediction){
  prediction <- as.numeric(prediction > 1)
  prediction <- matrix(prediction, ncol=11)
  prediction <- as.numeric(rowSums(prediction) > 0)
  
  CM = as.matrix(table(Actual = data$approved, Predicted = prediction))
  
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm = mean(c(TPR, TNR))
  return(ACCNorm)
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

sum(val$approved==1)
sum(val$approved==0)

sum(test$approved==1)
sum(test$approved==0)

## decision tree
library(rpart)

##ACC Vs Depth 
accPerDepth = data.frame(depth=numeric(28), accTrain=numeric(28), accVal=numeric(28), accTest=numeric(28))
for (maxDepth in 3:30){
  treeModel = rpart(formula=approved~ ., 
                    data=train, method="class",
                    control=rpart.control(minsplit=30, cp=0.008, maxdepth=maxDepth),
                    parms= list(split="information"))
  
  trainResults = predictAndEvaluateTree(treeModel, train)
  valResults = predictAndEvaluateTree(treeModel, val)
  testResults = predictAndEvaluateTree(treeModel, test)
  
  accPerDepth[maxDepth-2,] = c(maxDepth, trainResults$ACCNorm, valResults$ACCNorm, testResults$ACCNorm)
}

## Plot
library("reshape2")
library("ggplot2")
accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

treeModel = rpart(formula=approved ~ ., 
                  data=train, method="class",
                  control=rpart.control(minsplit=30, cp=0.008, maxdepth=8),
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

#$ grafico da accuracia normalizada pelo numero de arvores
type <- c(rep("accTrain",10),rep("accVal",10),rep("accTest",10))
graphic_data <- data.frame(ntree=rep(accPerNTree$ntree,3),
                           Dataset=c(accPerNTree$accTrain,accPerNTree$accVal,accPerNTree$accTest),
                           Group=type)
ggplot(data=graphic_data, aes(x=ntree, y=Dataset, colour=Group))+geom_line()+geom_point()


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

ACCNorm_train <- evaluateForestBagging(train,prediction_train)
ACCNorm_val <- evaluateForestBagging(val,prediction_val)
ACCNorm_test <- evaluateForestBagging(test,prediction_test)
ACCNorm_train
ACCNorm_val
ACCNorm_test


# other bagging
library(ipred)
model = ipredbagg(train[,"approved"], X=train[,1:30], nbagg=150)
baggTrain <- predictAndEvaluateForest(model, train) 
baggTrain$CM
baggTrain$ACCNorm

baggVal <- predictAndEvaluateForest(model, val) 
baggVal$CM
baggVal$ACCNorm

baggTest <- predictAndEvaluateForest(model, test) 
baggTest$CM
baggTest$ACCNorm

