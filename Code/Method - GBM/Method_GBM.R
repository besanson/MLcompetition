##-------------------------------------------------------------------------------------------------
##   GBM
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply the Gradient Boosting Method to the Cover Type Data


## USEFUL LINKS:
## http://cran.r-project.org/web/packages/h2o/h2o.pdf


## GENERAL INFO:
# If this code is run on a laptop use the commented h2o.init in line, 
# h2o library already runs it in parallel, so no extra coding is needed

##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------
source("Code/Utilities/Packages.R")

###LOAD PACKAGES
loadPackages(c("h2o","doParallel"))


##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------
rm(list=ls())
cl <- makeCluster(detectCores()-1) ## detect the cores in the machine
source("Code/Utilities/ReadData.R") ## all variables


##-------------------------------------------------------------------------------------------------
# Training
##-------------------------------------------------------------------------------------------------

set.seed(4321) 

# Fit Control Control the computational nuances of the train function

registerDoParallel(cl)
localH2O <- h2o.init()

###################################################
## If this code is to run on a Laptop, use:
##
#  localH2O <- h2o.init(ip = "localhost",
#                     port = 54321,
#                     startH2O = TRUE,
#                     max_mem_size = '2g',
#                     nthreads = -1)
###################################################

# Link dataset to h20 cluster
training_set_h2o <- as.h2o(localH2O, training_set, key = 'training_set')

## Previous parameter values used
# depth=8,12,16,18,20 
# trees =100, 250, 500, 1000
# minobsinnode= 1,5,10

# Train GBM model
trainedGbm <- h2o.gbm(x = 1:(ncol(training_set)-1),
                      y = ncol(training_set),
                      nfolds = 10, # cross validation
                      data = training_set_h2o,
                      interaction.depth = c(18),
                      n.trees = c(250),
                      n.minobsinnode = c(10),
                      shrinkage = c(0.1))

# Apply selected method to test data
testing_set_h2o <- as.h2o(localH2O, predict(preprocesamiento, testing_set), key = 'testing_set')

# Make prediction
ypredict <- h2o.predict(trainedGbm, testing_set_h2o)

# Stop cluster
stopCluster(cl)

##-------------------------------------------------------------------------------------------------
# Export Results
##-------------------------------------------------------------------------------------------------

output  <- data.frame(id = id_testing,
                       Cover_Type = gsub("Class_", "", as.data.frame(ypredict$predict)[[1]]))

write.table(output, "Code/Prediction/GBM.csv", sep = ",", row.names = FALSE, quote = FALSE)
