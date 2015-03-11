##-------------------------------------------------------------------------------------------------
##   GBM
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Random Forest to the Cover Type Data


## USEFULL LINKS:
## 


## GENERAL INFO:
# 


##-------------------------------------------------------------------------------------------------
rm(list=ls())

## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
if (!require("h2o")) install.packages("h2o")


##-------------------------------------------------------------------------------------------------
##INCLUDE DATA

training_set <- read.table("Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Kaggle_Covertype_test.csv", sep = ",", header = T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

# Looking if all the variables have variability
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #keep only those that have variability
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

# Doing some preprocess, as normalizing in this case
preprocesamiento <- preProcess(rbind(training_set[,-ncol(training_set)], testing_set),
                               method = c("center", "scale"))

##-------------------------------------------------------------------------------------------------
##   End of "preparing" the data, now the training begins
##-------------------------------------------------------------------------------------------------

#Step on the training set with the normalized data -where the Preprocess is applied through the function predict()-
#and adding the Cover_Type as a factor
training_set <- data.frame(predict(preprocesamiento, 
                                   training_set[,-ncol(training_set)]),
                           Cover_Type = paste("Class_", training_set$Cover_Type, sep = ""))

##-------------------------------------------------------------------------------------------------
# PREDICTIONS


set.seed(4321) ## we need to prove that this is "universal" so we set a new seed


#Fit Control Control the computational nuances of the train function

library(h2o)
localH2O <- h2o.init(ip = "localhost",
					 port = 54321,
					 startH2O = TRUE,
					 max_mem_size = '2g',
					 nthreads = -1)

training_set_h2o <- as.h2o(localH2O, training_set, key = 'training_set')

trainedGbm <- h2o.gbm(x = 1:(ncol(training_set)-1),
					  y = ncol(training_set),
					  nfolds = 3,
					  data = training_set_h2o,
					  interaction.depth = c(1, 2, 4, 8),
					  n.trees = c(250, 500, 750),
					  n.minobsinnode = c(10),
					  shrinkage = c(0.1))


testing_set_h2o <- as.h2o(localH2O, predict(preprocesamiento, testing_set), key = 'testing_set')

predicciones <- h2o.predict(trainedGbm, testing_set_h2o)

salidas  <- data.frame(id = id_testing,
					   Cover_Type = gsub("Class_", "", as.data.frame(predicciones$predict)[[1]]))

write.table(salidas, "Pred_Alessio_GBM.csv", sep = ",", row.names = FALSE, quote = FALSE)