##-------------------------------------------------------------------------------------------------
##   GBM
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Gradient Boosting Method to the Cover Type Data


## USEFULL LINKS:
## MARIA PLEASE


## GENERAL INFO:
# MARIA PLEASE



##-------------------------------------------------------------------------------------------------
rm(list=ls())

## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
#if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA

training_set <- read.table("Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Kaggle_Covertype_test.csv", sep = ",", header = T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

#Feature creation
#shift elevation by vertical distance
training_set$vertDistHyd_ElevShift <- (training_set$elevation -training_set$ver_dist_hyd)

#shift elevation by horizontal distance
# 0.6 was selected by choosing the value that reduced the skew of 
# classes 1 and 2 the most (to maximise this overlap group's
# separability
training_set$horDistHyd_ElevShift <- (training_set$elevation -training_set$hor_dist_hyd*0.6)

#add binary variable for horizontal hydro distance = 0
training_set$zeroHydDist<-0
training_set$zeroHydDist[training_set$hor_dist_hyd==0]<-1

#add binary variable for those sites with shade (darkness) at 3pm
training_set$shade3pm<-0
training_set$shade3pm[training_set$hill_3pm==0]<-1

#combine vertical and horizontal values
training_set$crowFliesHyd <- (training_set$hor_dist_hyd^2+training_set$ver_dist_hyd^2)^0.5

#specific variable that quantifies ability/speed to put out fire if one starts
#to be aligned with pine trees that have higher chance of fire
training_set$fire_risk <- training_set$crowFliesHyd * training_set$hor_dist_fire

###now same for test
testing_set$vertDistHyd_ElevShift <- (testing_set$elevation -testing_set$ver_dist_hyd)
testing_set$horDistHyd_ElevShift <- (testing_set$elevation -testing_set$hor_dist_hyd*0.6)
testing_set$zeroHydDist<-0
testing_set$zeroHydDist[testing_set$hor_dist_hyd==0]<-1
testing_set$shade3pm<-0
testing_set$shade3pm[testing_set$hill_3pm==0]<-1
testing_set$crowFliesHyd <- (testing_set$hor_dist_hyd^2+testing_set$ver_dist_hyd^2)^0.5
testing_set$fire_risk <- testing_set$crowFliesHyd * testing_set$hor_dist_fire

training_set<-cbind(training_set[, -55],Cover_Type=training_set[, 55]) #putting the Cover column last again

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
                           Cover_Type = as.factor(training_set$Cover_Type))

##-------------------------------------------------------------------------------------------------
# PREDICTIONS

set.seed(4321) ## we need to prove that this is "universal" so we set a new seed


#Fit Control Control the computational nuances of the train function

library(h2o)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

localH2O <- h2o.init()

##In case that is run in a Laptop use
#localH2O <- h2o.init(ip = "localhost",
#                     port = 54321,
#                     startH2O = TRUE,
#                     max_mem_size = '2g',
#                     nthreads = -1)

training_set_h2o <- as.h2o(localH2O, training_set, key = 'training_set')

trainedGbm <- h2o.gbm(x = 1:(ncol(training_set)-1),
                      y = ncol(training_set),
                      nfolds = 10,
                      data = training_set_h2o,
                      interaction.depth = c(18),
                      n.trees = c(250),
                      n.minobsinnode = c(10),
                      shrinkage = c(0.1))

#Actually for this last model... only 90 trees where needed.

testing_set_h2o <- as.h2o(localH2O, predict(preprocesamiento, testing_set), key = 'testing_set')

predicciones <- h2o.predict(trainedGbm, testing_set_h2o)

stopCluster(cl)

salidas  <- data.frame(id = id_testing,
                       Cover_Type = gsub("Class_", "", as.data.frame(predicciones$predict)[[1]]))

write.table(salidas, "pred_GBM_2.csv", sep = ",", row.names = FALSE, quote = FALSE)
