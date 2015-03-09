##-------------------------------------------------------------------------------------------------
##   Support Vector Machine
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Support Vector Machine to the Cover Type Data


## USEFULL LINKS:
## http://topepo.github.io/caret/Support_Vector_Machines.html


## GENERAL INFO:
# SVM depends on the method we choose the Kernel function to be in. We will have to choose 
# some different methods and see which one is best in this data set. Depending on the kind of 
# Kernel there will be some tuning parameters

##SVM function works directly with cross validation of 10-fold.

##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Machine Learning/MLcompetition")

cl <- makeCluster(detectCores()) ## detect the cores in the machine

## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
rm(list=ls())
training_set <- read.table("Data/Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Data/Kaggle_Covertype_test.csv", sep = ",", header = T)
testing_trueLabel<-read.table("Data/Kaggle_Covertype_sample.csv", sep=",",header=T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

# Looking if all the variables have variability
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #keep only those that have variability
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

# Doing some preprocess, as normalizing in this case
NormData <- preProcess(training_set[,-ncol(training_set)],
                               method = c("center", "scale"))
mm <- data.frame(predict(NormData,training_set[,-(ncol(training_set))]),
                 Cover_Type = as.factor(training_set$Cover_Type)) ##labels

##-------------------------------------------------------------------------------------------------
##   End of "preparing" the data, now the training beggins
##-------------------------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------
##   Doing some test  to see if the code works with only 10000 random observations

n<-nrow(training_set)
##preparing some data
idx <- seq(1:n)
idx <- idx[sample(1:n)]
##shuffel the data
data<-mm[idx[1:1000],]
##-------------------------------------------------------------------------------------------------
# So it's "replicable" we seet the seed.
#train control defines the method of the experiment.. number of folds
#fitControl <- trainControl(method = "cv",
                           #number = 5, #normally we use 10 groups for validating
                           #verboseIter = TRUE)
##Trying another package e1071:

## Linear -----------------------------------------------------------------------------
set.seed(4321)

registerDoParallel(cl)
Linear_svm<-tune(svm,Cover_Type~., data=data, kernel="linear", scale=FALSE,
                ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))

Linear_ypredict<-predict(Linear_svm$best.model,testing_set) #predict labels
Linear_error<-mean(testing_trueLabel[,2]==Linear_ypredict) #error of Linear

stopCluster(cl)


## Radial -----------------------------------------------------------------------------
registerDoParallel(cl)

Radial_svm<-tune(svm,Cover_Type~.,data=data, kernel="radial", scale=FALSE,
                ranges=list(cost=c(0.1,1,10,100,1000), ## different cost associated for the model
                            gamma=c(0.5,1,2,3,4))) ## different gammas for the model


Radial_ypredict<-predict(Radial_svm$best.model,testing_set) ##predict labels (best model)

Radial_error<-mean(testing_trueLabel[,2]==Radial_ypredict) ## error of Radial

##  -----------------------------------------------------------------------------
