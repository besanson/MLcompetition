##-------------------------------------------------------------------------------------------------
##   Random Forest
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
#if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA

training_set <- read.table("Data/Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Data/Kaggle_Covertype_test.csv", sep = ",", header = T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

# Looking if all the variables have variability
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #keep only those that have variability
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

# Doing some preprocess, as normalizing in this case
preprocesamiento <- preProcess(training_set[,-ncol(training_set)],
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
# In this case, a Cross Validation
fitControl <- trainControl(method = "cv",
                           number = 10,
                           verboseIter = TRUE)

# The Test 3 with this method had K fold = 5

#Parallel
#run model in parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)


# Random Forest
# Random Forest entreno el modelo... 
rfFit <- train(Cover_Type ~ ., data = training_set,
               method = "rf",
               trControl = fitControl,
               tuneGrid = expand.grid(mtry = c(20)),
               ntree = c(250))

# The Test 3 with this method had a Tune Grid of 20 and a ntree of 1000.
# A correct practitional approach would be Tune grid =sqrt(54)=7 and ntree between 100 and 500
# Test 4 Tune grid=8 and ntree=250. Result 0.81
# Test 5 Bagging tune grid = 53 and ntree=150 Result 0.89810

predicciones <- predict(rfFit, predict(preprocesamiento, testing_set))

stopCluster(cl)

salida <- data.frame(id =  id_testing, Cover_Type = predicciones)
write.table(salida, "Pred_Alessio_4.csv", sep = ",", row.names = FALSE, quote = FALSE)
