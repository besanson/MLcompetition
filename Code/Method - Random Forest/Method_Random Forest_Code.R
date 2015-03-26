##-------------------------------------------------------------------------------------------------
##   Random Forest
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Random Forest to the Cover Type Data


## USEFULL LINKS:
## http://caret.r-forge.r-project.org
## 

## GENERAL INFO:
# We have tried different parameters to see how the training error changed. The following code
# only includes the selected parameters.



##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------
source("Code/Packages.R")

###LOAD PACKAGES
loadPackages(c("caret", "e1071","doParallel"))

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------
rm(list=ls())


source("Code/ReadData.R")
cl <- makeCluster(detectCores()-1) ## detect the cores in the machine

##-------------------------------------------------------------------------------------------------
# Training
##-------------------------------------------------------------------------------------------------

set.seed(4321) ## we need to prove that this is "universal" so we set a new seed


#Fit Control Control the computational nuances of the train function
# In this case, a Cross Validation
fitControl <- trainControl(method = "cv",
                           number = 10,
                           verboseIter = TRUE)

#Parallel
#run model in parallel
registerDoParallel(cl)

# Random Forest with final model (the chosen parameters of different tryouts)
# other parameters that have being tried 
            #ntree= 1000
            #mtry= 8,16,24,32,40,44,56,59,48

rfFit <- train(Cover_Type ~ ., data = training_set,
               method = "rf",
               trControl = fitControl,
               tuneGrid =  data.frame(mtry = c(48)),ntree = c(1000))

ypredict <- predict(rfFit, testing_set)

stopCluster(cl)
##-------------------------------------------------------------------------------------------------
##   Output the results (different files might be created depending on the parameters selection)
##-------------------------------------------------------------------------------------------------

output <- data.frame(id =  id_testing, Cover_Type = ypredict)
write.table(output, "Code/Prediction/Pred_Alessio_RF.csv", sep = ",", row.names = FALSE, quote = FALSE)

