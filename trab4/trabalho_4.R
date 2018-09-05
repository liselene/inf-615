########################################
# Trabalho 4 - INF-615 
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
rm(list = ls())
#install.packages("neuralnet")
library(neuralnet)

#install.packages("e1071")
library(e1071)

library(ggplot2)

#setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab4")
#setwd("~/Projects/ComplexData/inf-615/trab4")

# create and process data
source("data_processing.R")
set.seed(42)

accplot<-function(ACCs_train,ACC_final_train, ACCs_val,ACC_final_val,label){

  ACCNorm <-  data.frame(number = factor(c(1:9, 0, "final", 1:9, 0, "final")),
                         ACC = factor(round(c(ACCs_train,ACC_final_train, ACCs_val,ACC_final_val), 2)),
                         type = factor(c(rep("treino", 11), rep("validação", 11))))
  
  ggplot(data = ACCNorm, aes(x = number, y = ACC, fill = type , adj = 1)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle(label, subtitle = NULL)
}

## svm routines
source("trabalho_4_svm.R")
accplot(ACCs_lin_train,ACC_final_lin_train, ACCs_lin_val,ACC_final_lin_val,"SVM Linear")
accplot(ACCs_rbf_train,ACC_final_rbf_train, ACCs_rbf_val,ACC_final_rbf_val,"SVM RBF")

## NN routines
source("trabalho_4_NN.R")
accplot(ACCs_train,ACC_final_train,ACCs_val,ACC_final_val, "Rede Neural")

## Test routines
source("trabalho_4_test.R")

