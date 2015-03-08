##-------------------------------------------------------------------------------------------------
##   Deep Learning
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
if (!require("h2o")) install.packages("h2o")

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


localH2O <- h2o.init()

training_set_h2o <- as.h2o(localH2O, training_set, key = 'training_set')


cl <- makeCluster(detectCores())
registerDoParallel(cl)

trained_dnnet <- h2o.deeplearning(x = 1:ncol(training_set), y = ncol(training_set),
                                  #checkpoint = trained_dnnet,
                                  data = training_set_h2o, activation = "RectifierWithDropout",
                                  classification = TRUE,
                                  loss = "CrossEntropy",
                                  hidden = c(1024, 1024, 1024),
                                  hidden_dropout_ratios = c(0.5, 0.5, 0.5),
                                  input_dropout_ratio = c(0),
                                  max_w2 = 50,
                                  l1 = 1e-5,
                                  nesterov_accelerated_gradient = TRUE,
                                  adaptive_rate = TRUE,
                                  epsilon = 1e-6,
                                  rho = 0.95,
                                  shuffle_training_data = TRUE,
                                  epochs = 100,
                                  holdout_fraction = 0.05, #nfolds = 5
                                  score_interval = 30,
                                  score_training_samples = 10000)

predicciones <- predict(trained_dnnet, predict(preprocesamiento, testing_set))

stopCluster(cl)

salida <- data.frame(id =  id_testing, Cover_Type = predicciones)
write.table(salida, "Pred_Alessio_4.csv", sep = ",", row.names = FALSE, quote = FALSE)