##-------------------------------------------------------------------------------------------------
##   Support Vector Machine
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply Support Vector Machine to the Cover Type Data


## USEFULL LINKS:
## http://topepo.github.io/caret/Support_Vector_Machines.html
## http://cran.r-project.org/web/packages/e1071/e1071.pdf


## GENERAL INFO:
# SVM depends on the method we choose the Kernel function to be in. We will have to choose 
# some different methods and see which one is best in this data set. Depending on the kind of 
# Kernel there will be some tuning parameters

##SVM function works directly with cross validation of 10-fold.

##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Machine Learning/MLcompetition")


## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("kernlab")) install.packages("kernlab")

if (!require("e1071")) install.packages("e1071")
if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
rm(list=ls())

cl <- makeCluster(detectCores()) ## detect the cores in the machine

training_set <- read.table("Data/Kaggle_Covertype_training.csv", sep = ",", header = T)
testing_set <- read.table("Data/Kaggle_Covertype_test.csv", sep = ",", header = T)
#testing_trueLabel<-read.table("Data/Kaggle_Covertype_sample.csv", sep=",",header=T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

##-------------------------------------------------------------------------------------------------
## New Variables:

Cover_Type<-training_set$Cover_Type
training_set<-training_set[,-ncol(training_set)]
training_set$vertDistHyd_ElevShift <- (training_set$elevation -training_set$ver_dist_hyd)

#shift elevation by horizontal distance

# 0.6 was selected by choosing the value that reduced the skew of 

# classes 1 and 2 the most (to maximise this overlap group's

# separability

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



###now same for test

testing_set$vertDistHyd_ElevShift <- (testing_set$elevation -testing_set$ver_dist_hyd)

testing_set$horDistHyd_ElevShift <- (testing_set$elevation -testing_set$hor_dist_hyd*0.6)

testing_set$zeroHydDist<-0

testing_set$zeroHydDist[testing_set$hor_dist_hyd==0]<-1

testing_set$shade3pm<-0

testing_set$shade3pm[testing_set$hill_3pm==0]<-1

testing_set$crowFliesHyd <- (testing_set$hor_dist_hyd^2+testing_set$ver_dist_hyd^2)^0.5

testing_set$fire_risk <- testing_set$crowFliesHyd * testing_set$hor_dist_fire

training_set$Cover_Type<-Cover_Type


# Looking if all the variables have variability
conVarianza <- apply(training_set, 2, function(x) sd(x) != 0) #keep only those that have variability
conVarianza <- names(conVarianza[conVarianza==T])

training_set <- training_set[, conVarianza]
testing_set  <- testing_set[, conVarianza[1:(length(conVarianza)-1)]]



# Doing some preprocess, as normalizing in this case
NormData <- preProcess(rbind(training_set[,-ncol(training_set)], testing_set),
                       method = c("center", "scale"))
training_set <- data.frame(predict(NormData,training_set[,-(ncol(training_set))]),
                           Cover_Type = as.factor(training_set$Cover_Type)) ##labels

testing_set<-predict(NormData,testing_set)




## Radial -----------------------------------------------------------------------------
#cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)

registerDoParallel(cl)


stopCluster(cl)




a<-svm(Cover_Type~.,data=training_set,cost=2, kernel="radial",scale=FALSE)

Radial_ypredict<-predict(a,testing_set) 
mean(dataTest[,ncol(dataTest)]!=Radial_ypredict)
table(Radial_ypredict,dataTest[,ncol(dataTest)])

mean(ypredict$mode!=test1label)

table(ypredict$mode,test1label)


results

ga<-0.1
ct<-100
tune(svm,Class~.,data=data[1:10000,],scale=F,
     class.weights = c(a=0.1,b=0.1,c=10,d=10,e=10,f=10,g=10),range=list(cost=c(0.1,1,10),gamma<-c(0.5,1,2)))


##  -----------------------------------------------------------------------------
## Save the results
##  -----------------------------------------------------------------------------

write.table(Radial_results, "Radial_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(Linear_results, "Linear_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)


##  -----------------------------------------------------------------------------


##-------------------------------------------------------------------------------------------------
##   End of "preparing" the data, now the training beggins
##-------------------------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------
##   Doing some test  to see if the code works with only 10000 random observations

n<-nrow(training_set)
##preparing some data
idx <- seq(1:n)
idx <- idx[sample(1:n)]
##shuffel the data
data<-training_set[idx,]


##Change the classes from number factor to letter factors, (weights don't work any other way)
classes<-c("a","b","c","d","e","f","g")

#Changing the label
for(i in 1:nrow(data)){
  current<-data$Cover_Type[i]
  data$Class[i]<-classes[current]
}

data$Class<-as.factor(data$Class)
data<-data[,-54] #taking out Cover_type
testlabel<-c()
test1label<-testing_trueLabel[,2]
for(i in 1:length(test1label)){
  current<-test1label[i]
  testlabel[i]<-classes[current]
}

testlabel<-as.factor(testlabel)