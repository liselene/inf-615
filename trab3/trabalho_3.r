########################################
# Trabalho 3 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())
setwd("~/Projects/ComplexData/inf-615/trab3")
#setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab3")

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

sum(val$approved==1)
sum(val$approved==0)

sum(test$approved==1)
sum(test$approved==0)

## decision tree
library(rpart)

##ACC Vs Depth 
accPerDepth = data.frame(depth=numeric(28), accTrain=numeric(28), accVal=numeric(28), accTest=numeric(28))
for (maxDepth in 3:30){
  print(maxDepth)
  treeModel = rpart(formula=approved~ ., 
                    data=train, method="class",
                    control=rpart.control(minsplit=10, cp=0.0001, maxdepth=maxDepth),
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
                  control=rpart.control(minsplit=10, cp=0.0001, maxdepth=13),
                  parms= list(split="information"))

#summary(treeModel)
#printcp(treeModel)

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

nTree = c(10,25, 50, 100, 500)
accPerNTree = data.frame(ntree=numeric(5), accTrain=numeric(5), accVal=numeric(5),accTest=numeric(5))
for (i in 1:5){
  rfModel = randomForest(formula=approved~ ., data= train, ntree=nTree[i])
  rfACCNormTrain = predictAndEvaluateForest(rfModel, train)
  rfACCNormVal = predictAndEvaluateForest(rfModel, val)
  rfACCNormTest = predictAndEvaluateForest(rfModel, test)
  accPerNTree[i,] = c(nTree[i], rfACCNormTrain$ACCNorm, rfACCNormVal$ACCNorm,rfACCNormTest$ACCNorm)
}

#$ grafico da accuracia normalizada pelo numero de arvores
type <- c(rep("accTrain",5),rep("accVal",5),rep("accTest",5))
graphic_data <- data.frame(ntree=rep(accPerNTree$ntree,3),
                           Dataset=c(accPerNTree$accTrain,accPerNTree$accVal,accPerNTree$accTest),
                           Group=type)
ggplot(data=graphic_data, aes(x=ntree, y=Dataset, colour=Group))+geom_line()


#install.packages("ipred")
library(ipred)
model = ipredbagg(train$approved, X=train, nbagg=1)
baggPrediction = predict(model, test) 
baggCM = as.matrix(table(Actual = test$approved, Predicted = baggPrediction))
baggTPR = baggCM[2,2] / (baggCM[2,2] + baggCM[2,1])
baggTNR = baggCM[1,1] / (baggCM[1,1] + baggCM[1,2])
baggACCNormVal = mean(c(baggTPR, baggTNR))


