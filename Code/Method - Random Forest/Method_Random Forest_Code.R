##-------------------------------------------------------------------------------------------------
##   Random Forest
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Random Forest to the Cover Type Data


## USEFULL LINKS:
## http://caret.r-forge.r-project.org
## Link to Packages in CRAN

## GENERAL INFO:
# We have tried different parameters to see how the training error changed. The following code
# only includes the selected parameters.

##GASTON WHAT ARE THE PARAMETERS?? number of trees? and lenght of each tree?


##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071") # Not sure if we are using this one
#if (!require("doMC")) install.packages("doMC")  # Not sure if we are using this one
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------
rm(list=ls())

cl <- makeCluster(detectCores()) ## detect the cores in the machine

source("Code/ReadData.R")

##-------------------------------------------------------------------------------------------------
##   End of "preparing" the data, now the training begins
##-------------------------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------
# PREDICTIONS
set.seed(4321) ## we need to prove that this is "universal" so we set a new seed


#Fit Control Control the computational nuances of the train function
# In this case, a Cross Validation
fitControl <- trainControl(method = "cv",
                           number = 10,
                           verboseIter = TRUE)

#Parallel
#run model in parallel
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

# Random Forest with final model (the chosen parameters of different tryouts)

rfFit <- train(Cover_Type ~ ., data = training_set,
               method = "rf",
               trControl = fitControl,
               tuneGrid =  data.frame(mtry = c(48)),ntree = c(1000))

ypredict <- predict(rfFit, predict(preprocesamiento, testing_set))

stopCluster(cl)
##-------------------------------------------------------------------------------------------------
##   Output the results (different files might be created depending on the parameters selection)
##-------------------------------------------------------------------------------------------------

output <- data.frame(id =  id_testing, Cover_Type = ypredict)
write.table(output, "Pred_Alessio_RF_Final_mtry48_trees1000_CV10.csv", sep = ",", row.names = FALSE, quote = FALSE)

