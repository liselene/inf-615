########################################
# Trabalho 3 - INF-615
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list=ls())
setwd("~/Projects/ComplexData/inf-615/trab3")
#setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab3")

predictAndEvaluate <- function(model, data){
  prediction = predict(model, data)
  prediction = as.numeric(prediction[,2] >= 0.5)
  
  CM = as.matrix(table(Actual = data$approved, Predicted = prediction))
  TPR = CM[2,2] / (CM[2,2] + CM[2,1])
  TNR = CM[1,1] / (CM[1,1] + CM[1,2])
  ACCNorm = mean(c(TPR, TNR))
  
  return(list(CM=CM, ACCNorm=ACCNorm))
}

## carregando os dados
train<-read.csv("student_performance_train.data")
val<-read.csv("student_performance_val.data")
test<-read.csv("student_performance_test_temp.data")

## inpecionando os dados
summary(train)
summary(val)
nrow(train) #626
nrow(val) #209
nrow(test) #??

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
accPerDepth = data.frame(depth=numeric(30), accTrain=numeric(30), accVal=numeric(30))
for (maxDepth in 3:30){
  treeModel = rpart(formula=approved~ ., 
                    data=train, method="class",
                    control=rpart.control(minsplit=10, cp=0.0001, maxdepth=maxDepth),
                    parms= list(split="information"))
  
  trainResults = predictAndEvaluate(treeModel, train)
  valResults = predictAndEvaluate(treeModel, val)
  testResults = predictAndEvaluate(treeModel, test)
  
  accPerDepth[maxDepth,] = c(maxDepth, trainResults$ACCNorm, valResults$ACCNorm, testResults$ACCNorm)
}

## Plot
library("ggplot2")
accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line()

treeModel = rpart(formula=approved ~ ., 
                  data=train, method="class",
                  control=rpart.control(minsplit=10, cp=0.0001, maxdepth=6),
                  parms= list(split="information"))

#summary(treeModel)
#printcp(treeModel)

##train
trainResults = predictAndEvaluate(treeModel, train)
trainResults$CM
trainResults$ACCNorm
#val
valResults = predictAndEvaluate(treeModel, val)
valResults$CM
valResults$ACCNorm
#test
testResults = predictAndEvaluate(treeModel, test)
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

  rfPrediction = predict(rfModel, train) 
  rfCM = as.matrix(table(Actual = train$approved, Predicted = rfPrediction))
  rfTPR = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
  rfTNR = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
  rfACCNormTrain = mean(c(rfTPR, rfTNR))
  
  rfPrediction = predict(rfModel, val) 
  rfCM = as.matrix(table(Actual = val$approved, Predicted = rfPrediction))
  rfTPR = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
  rfTNR = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
  rfACCNormVal = mean(c(rfTPR, rfTNR))
  
  rfPrediction = predict(rfModel, test) 
  rfCM = as.matrix(table(Actual = test$approved, Predicted = rfPrediction))
  rfTPR = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
  rfTNR = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
  rfACCNormTest = mean(c(rfTPR, rfTNR))
  
  accPerNTree[i,] = c(nTree[i], rfACCNormTrain, rfACCNormVal,rfACCNormTest)
}

#Confusion Matrix
rfPrediction = predict(rfModel, val) 
rfCM = as.matrix(table(Actual = val$approved, Predicted = rfPrediction))
rfTPR = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
rfTNR = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
rfACCNorm = mean(c(rfTPR, rfTNR))

library(ggplot2)
type <- c(rep("accTrain",nTree),rep("accVal",nTree),rep("accTest",nTree))
#graphic_data <- data.frame(Dataset = rep(nModels,2), Accuracy=c(all_ACCs,all_ACCsNorm),Group=type)
ggplot(accPerNTree,aes(x=accTrain, y=nTree, group=Group,colour=Group))+geom_line()


