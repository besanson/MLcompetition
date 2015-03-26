##-------------------------------------------------------------------------------------------------
##   Support Vector Machine
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Support Vector Machine to the Cover Type Data


## USEFULL LINKS:
## http://topepo.github.io/caret/Support_Vector_Machines.html
## http://cran.r-project.org/web/packages/e1071/e1071.pdf


## GENERAL INFO:
# SVM depends on the method we choose the Kernel function to be in. We will have to choose 
# some different methods and see which one is best in this data set. Depending on the kind of 
# Kernel there will be some tuning parameters

##SVM function works directly with cross validation of 10-fold.
## the following code tries different parameters to see how the training error variates. 
## Finally the parameters selected are using radial kernel
## cost=10, gamma=0.5

##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------


# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------
rm(list=ls())

cl <- makeCluster(detectCores()) ## detect the cores in the machine

source("Code/ReadData.R")

##-------------------------------------------------------------------------------------------------
##   Linear Kernel
##-------------------------------------------------------------------------------------------------

set.seed(4321)


Linear_costList<-c(0.001,0.01,0.1,1,5,10,100)
registerDoParallel(cl)

Linear_results<- foreach(cost = Linear_costList,.combine=rbind,.packages=c("e1071","doMC","caret")) %dopar% {
  Linear_svm<-svm(Cover_Type~., data=training_set, kernel="linear", scale=FALSE,
                  cost=cost)
  Linear_ypredict<-predict(Linear_svm) #predict labels
  Linear_error<-mean(training_set$Cover_Type!=Linear_ypredict) #error of Linear
  Linear_result <- c(cost, Linear_error)
}


##-------------------------------------------------------------------------------------------------
##   Radial Kernel
##-------------------------------------------------------------------------------------------------


Radial_costList<-c(0.1,1,10)
gammaList<-c(0.5,1,2)

Radial_costList<-rep(Radial_costList,length(gammaList))
Radial_gammaList<-c(rep(gammaList[1],3),rep(gammaList[2],3),rep(gammaList[3],3))


registerDoParallel(cl)
Radial_results<- foreach(cost = Radial_costList, gamma=Radial_gammaList, .combine=rbind,.packages=c("e1071","doMC","caret")) %dopar% {
  Radial_svm<-svm(Cover_Type~.,data=training_set, kernel="radial", scale=FALSE,
                  cost=cost, gamma=gamma)
  Radial_ypredict<-predict(Radial_svm) ##predict labels (best model)
  Radial_error<-mean(training_set$Cover_Type!=Radial_ypredict) #error of Linear
  Radial_result <- c(cost, gamma, Radial_error)
}

stopCluster(cl)


#Predictions with the selected parameters

Radial_svm<-svm(Cover_Type~.,data=training_set, kernel="radial", scale=FALSE,
                cost=10, gamma=0.5)
Radial_ypredict<-predict(Radial_svm,testing_set)


##  -----------------------------------------------------------------------------
## Save the results
##  -----------------------------------------------------------------------------
output <- data.frame(id =  id_testing, Cover_Type = Radial_ypredict)
write.table(output, "Code/Prediction/Pred_Alessio_SVM.csv", sep = ",", row.names = FALSE, quote = FALSE)

##  -----------------------------------------------------------------------------


