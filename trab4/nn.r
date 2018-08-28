#################################
# MDC - Machine Learning		#
# Neural Networks				#
#################################


# create toy examples
source("create_dataset.r")

# install and load nn package
install.packages("neuralnet")
library(neuralnet)


# Visualize the data
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c("Positive Train","Positive Test","Negative Train","Negative Test"),
    col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))




#NN only work with formula, so we need to cbind xtrain and ytrain
trainData = cbind(xtrain, ytrain)
colnames(trainData)= c("x1", "x2", "y")

trainData[1:3,]


# train a neuralnet
nnModel = neuralnet(formula="y ~ x1+x2", data=trainData, hidden=c(5,3), linear.output=FALSE) 
#linear.output = TRUE --> regression
#linear.output = FALSE --> classification (apply 'logistic' activation as default)


# General summary
summary(nnModel)

# Plot the network (the best through all repetitions)
plot(nnModel, rep="best")


#### Let's predict our test set
nnCompute = compute(nnModel, xtest)
nnCompute

prediction = nnCompute$net.result

prediction[prediction < 0.5] = -1
prediction[prediction >= 0.5] = 1

CM = as.matrix(table(Actual = ytest, Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))






####### Non-linear data #########

plot(nlx,col=ifelse(nly>0,1,2),pch=ifelse(nly>0,1,2))
legend("topleft",c('Positive','Negative'),col=c(1,2),pch=c(1,2),text.col=c(1,2))
grid()


#neuralnet only accepts formula
nlTrainData = cbind(nlxTrain, nlyTrain)
colnames(nlTrainData)= c("x1", "x2", "y")



# train a neuralnet
nnModel = neuralnet(formula="y ~ x1+x2", data=nlTrainData, hidden=c(5,3), linear.output=FALSE) 
#linear.output = TRUE --> regression
#linear.output = FALSE --> classification (apply 'logistic' activation as default)

nnCompute = compute(nnModel, nlxTest)

prediction = nnCompute$net.result

prediction[prediction < 0.5] = -1
prediction[prediction >= 0.5] = 1

CM = as.matrix(table(Actual = nlyTest, Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))

