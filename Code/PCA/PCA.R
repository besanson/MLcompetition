##-------------------------------------------------------------------------------------------------
##   PCA
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to create new variables and then try to apply PCA with all the variables


## USEFULL LINKS:
## 

## GENERAL INFO:
# 

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
library(scatterplot3d) 

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
##-------------------------------------------------------------------------------------------------

Cover_Type<-training_set$Cover_Type ## keep the type for later
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

##-------------------------------------------------------------------------------------------------
## Preprosesing:
##-------------------------------------------------------------------------------------------------


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

##-------------------------------------------------------------------------------------------------
## PCA
##-------------------------------------------------------------------------------------------------
# Take a random subset of the training data to test the different PCA:

n<-nrow(training_set)
##preparing some data
idx <- seq(1:n)
idx <- idx[sample(1:n)]
##shuffel the data
data<-training_set[idx[1:1500],]

##training linear kernel function

pca.linear<-prcomp(~.,data=data[,1:53])
pca.linear.variance<-pca.linear$sdev^2/sum(pca.linear$sdev^2)
pca.linear.cum.variance<-pca.linear.variance[1]
for(i in 2:length(pca.linear.variance)){
  pca.linear.cum.variance[i]<-pca.linear.variance[i]+pca.linear.cum.variance[i-1]
}


##training polynomial kernel function

pca.poly<-kpca(~.,data=data[,1:53],kernel="polydot",kpar=list(degree=4)) 
pca.poly.variance<-as.numeric(eig(pca.poly)/sum(eig(pca.poly)))
pca.poly.cum.variance<-pca.poly.variance[1]
for(i in 2:length(pca.poly.variance)){
  pca.poly.cum.variance[i]<-pca.poly.variance[i]+pca.poly.cum.variance[i-1]
}

##training  radial kernel function

pca.radial<-kpca(~.,data=data[,1:53],kernel="rbfdot",kpar=list(sigma=0.1)) 
pca.radial.variance<-as.numeric(eig(pca.radial)/sum(eig(pca.radial)))
pca.radial.cum.variance<-pca.radial.variance[1]
for(i in 2:length(pca.radial.variance)){
  pca.radial.cum.variance[i]<-pca.radial.variance[i]+pca.radial.cum.variance[i-1]
}


##plot the variance explained by the tree methods

exp.variance<-c(pca.linear.cum.variance[1:10],pca.poly.cum.variance[1:10],pca.radial.cum.variance[1:10])
exp.variance<-cbind(rep(seq(1,10,1),3),exp.variance)
exp.variance<-as.data.frame(exp.variance)
exp.variance$type<-c(rep("linear",10),rep("polynomial",10),rep("radial",10))
colnames(exp.variance)<-c("PC","Variance","Method")
exp.variance$PC<-as.integer(exp.variance$PC)

ggplot(exp.variance,aes(x=PC,y=Variance,colour=Method))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(1,10,1))+ylab("Explained Variance")

write.table(exp.variance,"Data/tables/PCA_Variance.csv",sep=";")


vectors<-pcv(pca)[,1:6]
vectors<-as.data.frame(vectors)
vectors$type<-train$Cover_Type


ksvm(type~.,data=vectors,C=2, kernel="rbfdot",scale=FALSE)

ksvm(Cover_Type~.,data=train,C=2,scale=FALSE)

with(vectors, scatterplot3d(V1, V2, V3, color = as.numeric(type), pch = 19))
with(train,scatterplot3d(elevation, hor_dist_hyd, ver_dist_hyd, color = as.numeric(Cover_Type), pch = 19))


