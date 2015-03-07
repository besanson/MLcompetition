rm(list=ls())
install.packages("caret")
install.packages("e1071")
install.packages("doMC")
install.packages("doParallel")
library(caret)
library(e1071)
#library(doMC)
library(doParallel)
#setwd("C:/Users/DON/Dropbox/ML Competition/MLcompetition")
#folder<-"C:/Users/DON/Dropbox/ML Competition/MLcompetition"



# Data
training_set <- read.table(file = "Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Kaggle_Covertype_test.csv", sep = ",", header = T)
id_testing <- testing_set$id
training_set <- training_set[,-1]
testing_set  <- testing_set[,-1]

# Hay variables que no tienen variabildiad. Todos Zeros o unos
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #con variance me quedo con las columas que teien varianza
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

preprocesamiento <- preProcess(training_set[,-ncol(training_set)],
                               method = c("center", "scale"))

###z-scores.... para arboles no hace nada.. sirve para SVM.. sirve para Redes Neurales

training_set <- data.frame(predict(preprocesamiento, 
                                   training_set[,-ncol(training_set)]),
                           Cover_Type = as.factor(training_set$Cover_Type))
###aplico prepocesamiento al training set sin la ultima columna.. y agrgo el cover_type como factor


# Hago las predicciones
set.seed(4321)


#train control define el metodo del experimento.. numer cantidad de folds
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