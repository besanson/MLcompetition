## Needs to be clean and introduce the parallell again

rm(list=ls())
library(caret)
library(doParallel)

setwd("C:/Users/DON/Dropbox/ML Competition/MLcompetition")
folder<-"C:/Users/DON/Dropbox/ML Competition/MLcompetition"


# Data
training_set <- read.table(file.path(folder, "/Data/Kaggle_Covertype_training.csv"),
                           sep = ",", header = T)
testing_set <- read.table(file.path(folder, "/Data/Kaggle_Covertype_test.csv"),
                          sep = ",", header = T)
id_testing <- testing_set$id
training_set <- training_set[,-1]
testing_set  <- testing_set[,-1]

conVarianza <- apply(training_set, 2, function(x) sd(x) != 0)
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

preprocesamiento <- preProcess(training_set[,-ncol(training_set)],
                               method = c("center", "scale"))

training_set <- data.frame(predict(preprocesamiento, 
                                   training_set[,-ncol(training_set)]),
                           Cover_Type = as.factor(training_set$Cover_Type))

# Hago las predicciones
set.seed(4321)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

# Random Forest
rfFit <- train(Cover_Type ~ ., data = training_set,
               method = "rf",
               trControl = fitControl,
               tuneGrid = expand.grid(mtry = c(20)),
               ntree = c(1000))

predicciones <- predict(rfFit, predict(preprocesamiento, testing_set))

stopCluster(cl)

salida <- data.frame(id =  id_testing, Cover_Type = predicciones)
write.table(salida, "predicciones.csv", sep = ",", row.names = FALSE, quote = FALSE)