##-------------------------------------------------------------------------------------------------
##   GBM
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Gradient Boosting Method to the Cover Type Data


## USEFULL LINKS:
## http://cran.r-project.org/web/packages/h2o/h2o.pdf


## GENERAL INFO:
# In used in a laptop run the commented h2o.init in line 
# h2o library already runs it in parallel, so no extra coding is needed

##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
#if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")
if (!require("h2o")) install.packages("h2o")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------
rm(list=ls())
cl <- makeCluster(detectCores()) ## detect the cores in the machine
source("Code/ReadData.R") ## all variables


##-------------------------------------------------------------------------------------------------
# Training
##-------------------------------------------------------------------------------------------------

set.seed(4321) 

#Fit Control Control the computational nuances of the train function

registerDoParallel(cl)

localH2O <- h2o.init()

##In case that is run in a Laptop use
#localH2O <- h2o.init(ip = "localhost",
#                     port = 54321,
#                     startH2O = TRUE,
#                     max_mem_size = '2g',
#                     nthreads = -1)

training_set_h2o <- as.h2o(localH2O, training_set, key = 'training_set')


## We trained with depth=8,12,16,18,20 
                  # trees =100, 250, 500, 1000
                  # minobsinnode= 1,5,10

trainedGbm <- h2o.gbm(x = 1:(ncol(training_set)-1),
                      y = ncol(training_set),
                      nfolds = 10, # cross validation
                      data = training_set_h2o,
                      interaction.depth = c(18),
                      n.trees = c(250),
                      n.minobsinnode = c(10),
                      shrinkage = c(0.1))

# applying the selected method to the testing data set
testing_set_h2o <- as.h2o(localH2O, predict(preprocesamiento, testing_set), key = 'testing_set')
# predict the labels
ypredict <- h2o.predict(trainedGbm, testing_set_h2o)

stopCluster(cl)

##-------------------------------------------------------------------------------------------------
# Export Results
##-------------------------------------------------------------------------------------------------

output  <- data.frame(id = id_testing,
                       Cover_Type = gsub("Class_", "", as.data.frame(ypredict$predict)[[1]]))

write.table(output, "Code/Prediction/GBM.csv", sep = ",", row.names = FALSE, quote = FALSE)
