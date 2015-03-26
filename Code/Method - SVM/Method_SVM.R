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
# Kernel there will be different tuning parameters.

##SVM function works directly with cross validation of 10-fold.
## the following code tries different parameters to see how the training error variates. 
## Finally the parameters selected are using radial kernel
## cost=10, gamma=0.5

# Clear environment
rm(list=ls())

##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------
source("Code/Utilities/Packages.R")

###LOAD PACKAGES
loadPackages(c("caret", "e1071","doParallel"))

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------

source("Code/Utilities/ReadData.R")
cl <- makeCluster(detectCores()-1) ## detect the cores in the machine


##-------------------------------------------------------------------------------------------------
##   Radial Kernel
##-------------------------------------------------------------------------------------------------

# Choosing cost and gamma paramaters we are going to test
Radial_costList<-c(0.1,1,10)
gammaList<-c(0.5,1,2)

# Transform for use in parallel
Radial_costList<-rep(Radial_costList,length(gammaList))
Radial_gammaList<-c(rep(gammaList[1],3),rep(gammaList[2],3),rep(gammaList[3],3))

# Register cluster
registerDoParallel(cl)

# Function to produce error of SVM utilizing parameter grid
Radial_results<- foreach(cost = Radial_costList, gamma=Radial_gammaList, .combine=rbind,.packages=c("e1071","doMC","caret")) %dopar% {
  Radial_svm<-svm(Cover_Type~.,data=training_set, kernel="radial", scale=FALSE,
                  cost=cost, gamma=gamma)
  Radial_ypredict<-predict(Radial_svm) ##predict labels (best model)
  Radial_error<-mean(training_set$Cover_Type!=Radial_ypredict) #error of Linear
  Radial_result <- c(cost, gamma, Radial_error)
}

stopCluster(cl)

Radial_results<-as.data.frame(Radial_results)
colnames(Radial_results)<-c("cost","gamma","error")

# Predictions with the selected parameters
Radial_svm<-svm(Cover_Type~.,data=training_set, kernel="radial", scale=FALSE,
                cost=10, gamma=0.5)
Radial_ypredict<-predict(Radial_svm,testing_set)


##  -----------------------------------------------------------------------------
## Save the results
##  -----------------------------------------------------------------------------
output <- data.frame(id =  id_testing, Cover_Type = Radial_ypredict)
write.table(output, "Code/Prediction/Pred_Alessio_SVM.csv", sep = ",", row.names = FALSE, quote = FALSE)

##  -----------------------------------------------------------------------------


