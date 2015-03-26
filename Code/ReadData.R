##-------------------------------------------------------------------------------------------------
##  Read Data
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to read the data and add some new variables we consider will help our classification


## GENERAL INFO:

# this code reads and normalizes the data, only sourcing this code in the others we will not need
# to retype this in every code.

# only put in other codes 
# source("Code/ReadData.R")

##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Machine Learning/MLcompetition")


if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
##-------------------------------------------------------------------------------------------------

rm(list=ls())


training_set <- read.table("Data/Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Data/Kaggle_Covertype_test.csv", sep = ",", header = T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]
or.training_set<-training_set
or.testing_set<-testing_set


##-------------------------------------------------------------------------------------------------
## New Variables
##-------------------------------------------------------------------------------------------------

##New Variables

Cover_Type<-training_set$Cover_Type#keep the cover type
training_set<-training_set[,-ncol(training_set)]

#shift elevation by vertical distance
training_set$vertDistHyd_ElevShift <- (training_set$elevation -training_set$ver_dist_hyd)

#shift elevation by horizontal distance
# 0.6 was selected by choosing the value that reduced the skew of 
# classes 1 and 2 the most (to maximise this overlap group's separability
training_set$horDistHyd_ElevShift <- (training_set$elevation -training_set$hor_dist_hyd*0.6)

#add binary variable for horizontal hydro distance = 0
training_set$zeroHydDist<-0
training_set$zeroHydDist[training_set$hor_dist_hyd==0]<-1

#add binary variable for those sites with shade (darkness) at 3pm
training_set$shade3pm<-0
training_set$shade3pm[training_set$hill_3pm==0]<-1

#combine vertical and horizontal values
training_set$crowFliesHyd <- (training_set$hor_dist_hyd^2+training_set$ver_dist_hyd^2)^0.5


#specific variable that quantifies ability/speed to put out fire if one starts
#to be aligned with pine trees that have higher chance of fire
training_set$fire_risk <- training_set$crowFliesHyd * training_set$hor_dist_fire



###The same variables for the test data

testing_set$vertDistHyd_ElevShift <- (testing_set$elevation -testing_set$ver_dist_hyd)
testing_set$horDistHyd_ElevShift <- (testing_set$elevation -testing_set$hor_dist_hyd*0.6)
testing_set$zeroHydDist<-0
testing_set$zeroHydDist[testing_set$hor_dist_hyd==0]<-1
testing_set$shade3pm<-0
testing_set$shade3pm[testing_set$hill_3pm==0]<-1
testing_set$crowFliesHyd <- (testing_set$hor_dist_hyd^2+testing_set$ver_dist_hyd^2)^0.5
testing_set$fire_risk <- testing_set$crowFliesHyd * testing_set$hor_dist_fire

#include again the cover type in the training data
training_set$Cover_Type<-Cover_Type

##-------------------------------------------------------------------------------------------------
## Some scaling
##-------------------------------------------------------------------------------------------------

# Looking if all the variables have variability
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #keep only those that have variability
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]

# Doing some preprocess, as normalizing in this case
#NormData <- preProcess(rbind(training_set[,-ncol(training_set)], testing_set),
                       #method = c("center", "scale"))
NormData <- preProcess(rbind(training_set[,-c(11:53,56,57,60)], testing_set[,-c(11:53,56,57)]),
                       method = c("center", "scale"))

training_set <- data.frame(predict(NormData,training_set[,-c(11:53,56,57,60)]),
                            training_set[,c(11:53,56,57)],
                           Cover_Type = as.factor(training_set$Cover_Type)) ##labels

testing_set<-data.frame(predict(NormData,testing_set[,-c(11:53,56,57)]),
                        testing_set[,c(11:53,56,57)])

##Cleaning the not important values
rm(conVarianza)
rm(Cover_Type)
rm(NormData)

