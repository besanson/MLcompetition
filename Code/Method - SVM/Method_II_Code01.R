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

##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Machine Learning/MLcompetition")


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

cl <- makeCluster(detectCores()) ## detect the cores in the machine

training_set <- read.table("Data/Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Data/Kaggle_Covertype_test.csv", sep = ",", header = T)
#testing_trueLabel<-read.table("Data/Kaggle_Covertype_sample.csv", sep=",",header=T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

# Looking if all the variables have variability
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #keep only those that have variability
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

# Doing some preprocess, as normalizing in this case
NormData <- preProcess(rbind(training_set[,-ncol(training_set)], testing_set),
                       method = c("center", "scale"))
training_set <- data.frame(predict(NormData,training_set[,-(ncol(training_set))]),
                 Cover_Type = as.factor(training_set$Cover_Type)) ##labels

testing_set<-predict(NormData,testing_set)

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
data<-training_set[idx,]

##setting the cost per class
costs <- table(data$Cover_Type)
costs<-1-costs/sum(costs)

##-------------------------------------------------------------------------------------------------
##   Linear Kernel
##-------------------------------------------------------------------------------------------------

set.seed(4321)


Linear_costList<-c(0.001,0.01,0.1,1,5,10,100)
registerDoParallel(cl)

Linear_results<- foreach(cost = Linear_costList,.combine=rbind,.packages=c("e1071","doMC","caret")) %dopar% {
  Linear_svm<-svm(Cover_Type~., data=data, kernel="linear", scale=FALSE,
                  cost=cost)
  Linear_ypredict<-predict(Linear_svm) #predict labels
  Linear_error<-mean(data$Cover_Type!=Linear_ypredict) #error of Linear
  Linear_result <- c(cost, Linear_error)
}


Linear_results

##-------------------------------------------------------------------------------------------------
##   Radial Kernel
##-------------------------------------------------------------------------------------------------


Radial_costList<-c(0.1,1,10)
gammaList<-c(0.5,1,2)

Radial_costList<-rep(Radial_costList,length(gammaList))
Radial_gammaList<-c(rep(gammaList[1],3),rep(gammaList[2],3),rep(gammaList[3],3))


registerDoParallel(cl)
Radial_results<- foreach(cost = Radial_costList, gamma=Radial_gammaList, .combine=rbind,.packages=c("e1071","doMC","caret")) %dopar% {
  Radial_svm<-svm(Cover_Type~.,data=data, kernel="radial", scale=FALSE,
                  cost=cost, gamma=gamma)
  Radial_ypredict<-predict(Radial_svm) ##predict labels (best model)
  Radial_error<-mean(data$Cover_Type!=Radial_ypredict) #error of Linear
  Radial_result <- c(cost, gamma, Radial_error)
}

stopCluster(cl)
Radial_results

##-------------------------------------------------------------------------------------------------
##   Polynomial Kernel
##-------------------------------------------------------------------------------------------------

Poly_costList<-c(0.1,1,10)
degree<-c(2,3,4)

Poly_costList<-rep(Poly_costList,length(degree))
Poly_degreeList<-c(rep(degree[1],3),rep(degree[2],3),rep(degree[3],3))

Poly_results<- foreach(cost = Poly_costList, degree=Poly_degreeList, .combine=rbind,.packages=c("e1071","doMC","caret")) %dopar% {
  Poly_svm<-svm(Cover_Type~.,data=data, kernel="polynomial", scale=FALSE,
                  cost=cost, degree=degree)
  Poly_ypredict<-predict(Poly_svm) ##predict labels (best model)
  Poly_error<-mean(data$Cover_Type!=Poly_ypredict) #error of Linear
  Poly_result <- c(cost, degree, Poly_error)
}

Poly_results



stopCluster(cl)

##  -----------------------------------------------------------------------------
## Save the results
##  -----------------------------------------------------------------------------

write.table(Radial_results, "Radial_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(Linear_results, "Linear_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)


##  -----------------------------------------------------------------------------


