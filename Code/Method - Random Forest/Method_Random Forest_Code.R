##-------------------------------------------------------------------------------------------------
##   Random Forest
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Random Forest to the Cover Type Data


## USEFULL LINKS:
## http://caret.r-forge.r-project.org

## GENERAL INFO:
# We have tried different parameters to see how the training error changed. The following code
# only includes the selected parameters.



##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------
## Clear environment
rm(list=ls())
if (!require("e1071")) install.packages("e1071")
if (!require("caret")) install.packages("caret")
if (!require("doParallel")) install.packages("doParallel")


##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------

source("Code/Utilities/ReadData.R")
cl <- makeCluster(detectCores()-1) ## detect the cores in the machine

##-------------------------------------------------------------------------------------------------
# Training
##-------------------------------------------------------------------------------------------------

set.seed(4321) ## we need to prove that this is "universal" so we set a new seed


# Fit Control controls the computational nuances of the train function
# ...in this case, through Cross Validation
fitControl <- trainControl(method = "cv",
                           number = 10,
                           verboseIter = TRUE)

# Run model in parallel
registerDoParallel(cl)



## Previous parameters that have being tried 
# ntree= 1000
# mtry= 8,16,24,32,40,44,56,59,48

## Random Forest with final model (optimal parameters from previous analyses)
rfFit <- train(Cover_Type ~ ., data = training_set,
               method = "rf",
               trControl = fitControl,
               tuneGrid =  data.frame(mtry = c(48)),ntree = c(1000))

# Generate prediction
ypredict <- predict(rfFit, testing_set)

stopCluster(cl)

##-------------------------------------------------------------------------------------------------
##   Output the results (different files might be created depending on the parameters selection)
##-------------------------------------------------------------------------------------------------

output <- data.frame(id =  id_testing, Cover_Type = ypredict)
write.table(output, "Code/Prediction/Pred_Alessio_RF.csv", sep = ",", row.names = FALSE, quote = FALSE)

