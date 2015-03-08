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
##   End of "preparing" the data, now the training beggins
##-------------------------------------------------------------------------------------------------

###z-scores.... para arboles no hace nada.. sirve para SVM.. sirve para Redes Neurales


##Maria: NO LO ENTIENDO
training_set <- data.frame(predict(preprocesamiento, 
                                   training_set[,-ncol(training_set)]),
                           Cover_Type = as.factor(training_set$Cover_Type))
###aplico prepocesamiento al training set sin la ultima columna.. y hago el cover_type como factor

##-------------------------------------------------------------------------------------------------
# PREDICTIONS


set.seed(4321) ## we need to prove that this is "universal" so we set a new seed


#train control defines the method of the experiment.. number of folds
fitControl <- trainControl(method = "cv",
                           number = 5,
                           verboseIter = TRUE)


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
               ntree = c(1000))

#tunegrid..... que parametros...raiz cuadarada de la cantidad de variables...
#random forest no hace overfiting...
#tuneGrid = expand.grid(mtry = c(5, 10, 20, 50)),
#ntree = c(250)

predicciones <- predict(rfFit, predict(preprocesamiento, testing_set))

stopCluster(cl)

salida <- data.frame(id =  id_testing, Cover_Type = predicciones)
write.table(salida, "Pred_Alessio_3.csv", sep = ",", row.names = FALSE, quote = FALSE)