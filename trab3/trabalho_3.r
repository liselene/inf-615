########################################
# Trabalho 3 - INF-615
# Nome(s): Liselene Borges e 
########################################
rm(list=ls())
#setwd("~/Projects/ComplexData/inf-615/trab3")
setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab3")

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

## inpecionando os dados
summary(train)
summary(val)
nrow(train) #626
nrow(val) #209

## intervalo das features
#sapply(train, max)
#sapply(train, min)

train$approved = as.factor(train$approved)
val$approved = as.factor(val$approved)

## quantidade de exemplos de cada classe
sum(train$approved==1)
sum(train$approved==0)

# DECISION TREE MODELS
library(rpart)
#If we want to use Entropy + Gain of Information
treeModel = rpart(formula=approved ~ ., 
                  data=train, method="class",
                  parms= list(split="information"))


#If we want to use Gini to select features
treeModel = rpart(formula=approved~ ., 
                  data=train, method="class",
                  parms= list(split="gini"))

#Allowing it to grow
treeModel = rpart(formula=approved~ ., 
                  data=train, method="class",
                  control=rpart.control(minsplit=10, cp=0.0001),
                  parms= list(split="information"))

summary(treeModel)

#Save the complete DT into file
post(treeModel, file = "tree2.ps",title = "Classification Tree for Income")




######### POST PRUNE ########

#Print the table with complexity parameters
printcp(treeModel)

#Prune the tree based on the complexity parameter that minimizes 
#the error in cross-validation (xerror)
minCP = treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"]

ptree = prune(treeModel, cp=minCP)
summary(ptree)


treeEval = predictAndEvaluate(treeModel, val)
treeEval$CM
treeEval$ACCNorm

#prunned tree
ptreeEval = predictAndEvaluate(ptree, val)
ptreeEval$CM
ptreeEval$ACCNorm


########## ACC Vs Depth 
# Let's see how the acc varies as we increase the tree's depth
accPerDepth = data.frame(depth=numeric(30), accTrain=numeric(30), accVal=numeric(30))
for (maxDepth in 3:30){
  treeModel = rpart(formula=approved~ ., 
                    data=train, method="class",
                    control=rpart.control(minsplit=10, cp=0.0001, maxdepth=maxDepth),
                    parms= list(split="information"))
  
  trainResults = predictAndEvaluate(treeModel, train)
  valResults = predictAndEvaluate(treeModel, val)
  
  accPerDepth[maxDepth,] = c(maxDepth, trainResults$ACCNorm, valResults$ACCNorm)
}

#Plot
library("reshape2")
library("ggplot2")

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format

ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line()





############# RANDOM FOREST
#install.packages('randomForest')
library(randomForest)
#help(randomForest)


#Train RF model
rfModel = randomForest(formula=approved~ ., data=train, ntree=400, type = "classification")


#Plotting the error
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rfModel, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfModel$err.rate),col=1:4,cex=0.8,fill=1:4)




#Confusion Matrix
rfPrediction = predict(rfModel, val) 
rfCM = as.matrix(table(Actual = val$approved, Predicted = rfPrediction))
rfTPR = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
rfTNR = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
rfACCNorm = mean(c(rfTPR, rfTNR))




nTree = c(10,25, 50, 100, 500)
accPerNTree = data.frame(ntree=numeric(5), accTrain=numeric(5), accVal=numeric(5))
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
  
  accPerNTree[i,] = c(nTree[i], rfACCNormTrain, rfACCNormVal)
}




