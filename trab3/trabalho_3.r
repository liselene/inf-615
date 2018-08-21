########################################
# Trabalho 3 - INF-615
# Nome(s): Liselene Borges e 
########################################
rm(list=ls())
setwd("~/Projects/ComplexData/inf-615/trab3")

## carregando os dados
train<-read.csv("student_performance_train.data")
val<-read.csv("student_performance_val.data")

## inpecionando os dados
summary(train)
summary(val)
nrow(train) #626
nrow(val) #209

## range dos dados
boxplot(train[train$approved==1,-31], 
        main="Range dos dados dos aprovados", 
        xlab="Tipo", ylab="Grandeza")

boxplot(train[train$approved==0,-31], 
        main="Range dos dados dos reprovados", 
        xlab="Tipo", ylab="Grandeza")

## discretização dos dados
train$schoolGP = as.numeric(train$school == "GP")
train$school = NULL

train$sexF = as.numeric(train$sex == "F")
train$sex = NULL

train$addressU = as.numeric(train$address == "U")
train$address = NULL

train$famSizeGT3 = as.numeric(train$famsize == "GT3")
train$famsize = NULL

train$PstatusA = as.numeric(train$Pstatus == "A")
train$Pstatus = NULL

levels <- unique(train$Mjob)
train$MJob_other = as.numeric(train$Mjob == levels[1])
train$MJob_services = as.numeric(train$Mjob == levels[2])
train$MJob_teacher = as.numeric(train$Mjob == levels[3])
train$MJob_health = as.numeric(train$Mjob == levels[4])
train$Mjob = NULL

levels <- unique(train$Fjob)
train$FJob_other = as.numeric(train$Fjob == levels[1])
train$FJob_services = as.numeric(train$Fjob == levels[2])
train$FJob_teacher = as.numeric(train$Fjob == levels[3])
train$FJob_health = as.numeric(train$Fjob == levels[4])
train$Fjob = NULL

levels <- unique(train$reason)
train$reason_reputation = as.numeric(train$reason == levels[1])
train$reason_home = as.numeric(train$reason == levels[2])
train$reason_course = as.numeric(train$reason == levels[3])
train$reason = NULL

levels <- unique(train$guardian)
train$guardian_father = as.numeric(train$guardian == levels[1])
train$guardian_mother = as.numeric(train$guardian == levels[2])
train$guardian = NULL

train$schoolsup_yes = as.numeric(train$schoolsup == "yes")
train$schoolsup = NULL

train$famsup_yes = as.numeric(train$famsup == "yes")
train$famsup = NULL

train$paid_yes = as.numeric(train$paid == "yes")
train$paid = NULL

train$activities_yes = as.numeric(train$activities == "yes")
train$activities = NULL

train$nursery_yes = as.numeric(train$nursery == "yes")
train$nursery = NULL

train$higher_yes = as.numeric(train$higher == "yes")
train$higher = NULL

train$internet_yes = as.numeric(train$internet == "yes")
train$internet = NULL

train$romantic_yes = as.numeric(train$romantic == "yes")
train$romantic = NULL

## intervalo das features
sapply(train, max)
sapply(train, min)

## quantidade de exemplos de cada classe
sum(train$approved==1)
sum(train$approved==0)

## normaliza !!!!!ver quais features precisam!!!!
#train_mean <- colMeans(train[,1:11]) #mean of each feature
#train_sd  <- apply(train[,1:11], 2, sd) #std of each feature

#train[,1:11] = sweep(train[,1:11], 2, train_mean, "-")
#train[,1:11] = sweep(train[,1:11], 2, train_sd, "/")
