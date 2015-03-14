##-------------------------------------------------------------------------------------------------
##   PCA
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to create new variables and then try to apply PCA with all the variables


## USEFULL LINKS:
## http://www.jstatsoft.org/v11/i09/paper
## http://stackoverflow.com/questions/14925942/eigenvalues-vs-pve-percent-variance-explained
## http://cran.r-project.org/web/packages/kernlab/kernlab.pdf

## GENERAL INFO:
# The PCA looks for the correlation between different variables of our data, and aims to obtain
# a subset of support vector that includes the maximum of variance of the original data.
# We are going to apply 3 transformation type: linear, polynomial and radial transformation.

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

##training linear kernel function.........................................................

pca.linear<-prcomp(~.,data=data[,1:53])
pca.linear.variance<-pca.linear$sdev^2/sum(pca.linear$sdev^2)
pca.linear.cum.variance<-pca.linear.variance[1]
for(i in 2:length(pca.linear.variance)){
  pca.linear.cum.variance[i]<-pca.linear.variance[i]+pca.linear.cum.variance[i-1]
}


##training polynomial kernel function....................................................

pca.poly<-kpca(~.,data=data[,1:53],kernel="polydot",kpar=list(degree=2)) 
pca.poly.variance<-as.numeric(eig(pca.poly)/sum(eig(pca.poly)))
pca.poly.cum.variance<-pca.poly.variance[1]
for(i in 2:length(pca.poly.variance)){
  pca.poly.cum.variance[i]<-pca.poly.variance[i]+pca.poly.cum.variance[i-1]
}

##training  radial kernel function.......................................................

pca.radial<-kpca(~.,data=data[,1:53],kernel="rbfdot",kpar=list(sigma=0.1)) 
pca.radial.variance<-as.numeric(eig(pca.radial)/sum(eig(pca.radial)))
pca.radial.cum.variance<-pca.radial.variance[1]
for(i in 2:length(pca.radial.variance)){
  pca.radial.cum.variance[i]<-pca.radial.variance[i]+pca.radial.cum.variance[i-1]
}

##-------------------------------------------------------------------------------------------------
## Results
##-------------------------------------------------------------------------------------------------


## 1. plot the variance explained by the tree methods
##-------------------------------------------------------------------------------------------------

exp.variance<-c(pca.linear.cum.variance[1:10],pca.poly.cum.variance[1:10],pca.radial.cum.variance[1:10])
exp.variance<-cbind(rep(seq(1,10,1),3),exp.variance)
exp.variance<-as.data.frame(exp.variance)
exp.variance$type<-c(rep("linear",10),rep("polynomial",10),rep("radial",10))
colnames(exp.variance)<-c("PC","Variance","Method")
exp.variance$PC<-as.integer(exp.variance$PC)

ggplot(exp.variance,aes(x=PC,y=Variance,colour=Method))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(1,10,1))+ylab("Explained Variance")


##Export Results:     ----- . ----- . ----- . ----- . ----- . ----- . ----- .
write.table(exp.variance,"Data/tables/PCA_Variance.csv",sep=";")
png(filename="Report/Graphs/PCA_Variance", width = 1100, height = 700)
ggplot(exp.variance,aes(x=PC,y=Variance,colour=Method))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(1,10,1))+ylab("Explained Variance")
dev.off()
## End of Exporting   ----- . ----- . ----- . ----- . ----- . ----- . ----- .



## 2. Plot the error using SVM in all the data and with PCA of polynomial
##-------------------------------------------------------------------------------------------------


error<-c()
for(i in 1:30){ ## loop to see the evolution of the training error, as we include more PC
  vectors<-pcv(pca.poly)[,1:i]
  vectors<-as.data.frame(vectors)
  vectors$type<-data$Cover_Type
  svm.poly<-ksvm(type~.,data=vectors,C=2, kernel="rbfdot",scale=FALSE)
  error[i]<-error(svm.poly)
}

svm<-ksvm(Cover_Type~.,data=data,kernel="rbfdot",C=2,scale=FALSE) ## error of SVM with 53 variables
error.svm<-error(svm)

error<-cbind(seq(1,30,1),error)
error.svm<-(cbind(seq(1,30,1),rep(error.svm,30)))
error<-rbind(error,error.svm)
error<-as.data.frame(error)
colnames(error)<-c("PC","error")
method<-c(rep("PCA",30),rep("NON-PCA",30))
error$type<-as.factor(method)


ggplot(error,aes(x=PC,y=error,colour=type))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(0,30,5))

##Export Results:     ----- . ----- . ----- . ----- . ----- . ----- . ----- .
write.table(error,"Data/tables/PCA_error.csv",sep=";")
png(filename="Report/Graphs/PCA_error", width = 1100, height = 700)
ggplot(error,aes(x=PC,y=error,colour=type))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(0,30,5))
dev.off()
## End of Exporting   ----- . ----- . ----- . ----- . ----- . ----- . ----- .


