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
data<-mm[idx[1:10000],]
##-------------------------------------------------------------------------------------------------

# So it's "replicable" we seet the seed.
set.seed(4321)

#train control defines the method of the experiment.. number of folds
fitControl <- trainControl(method = "cv",
                           number = 5, #normally we use 10 groups for validating
                           verboseIter = TRUE)


###From this moment on I don't understand the errors

#Parallel
#run model in parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)


# SVM
# SVM train the model
rfFit <- train(y=data$Cover_Type , x = data[,-ncol(data)],
               method = "lssvmRadial",
               trControl = fitControl,
               sigma=0.8)##using the CV validation

#tunegrid..... que parametros...raiz cuadarada de la cantidad de variables...
#random forest no hace overfiting...
#tuneGrid = expand.grid(mtry = c(5, 10, 20, 50)),
#ntree = c(250)

predicciones <- predict(rfFit, predict(NormData, testing_set[1:1000,]))
stopCluster(cl)


##test the error
table(predicciones, testing_trueLabel[1:1000,2])

mean(testing_trueLabel[1:1000,2]==predicciones)



salida <- data.frame(id =  id_testing, Cover_Type = predicciones)
write.table(salida, "Pred_Alessio_3.csv", sep = ",", row.names = FALSE, quote = FALSE)