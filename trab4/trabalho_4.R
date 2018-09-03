########################################
# Trabalho 4 - INF-615 
# Nome(s): Liselene Borges e Marcos Scarpim
########################################
#install.packages("neuralnet")
library(neuralnet)

#install.packages("e1071")
library(e1071)

#setwd("~/Documents/UNICAMP/Curso - Mineracao/INF-0615/inf-615/trab4")
#setwd("~/Projects/ComplexData/inf-615/trab4")

# create and process data
source("data_processing.R")
set.seed(42)

## svm routines
source("trablho_4_svn.R")

## NN routines
source("trablho_4_NN.R")

## Test routines
source("trablho_4_test.R")

