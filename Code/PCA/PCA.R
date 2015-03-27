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
rm(list=ls())
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

source("Code/ReadData.R")

cl <- makeCluster(detectCores()) ## detect the cores in the machine


##-------------------------------------------------------------------------------------------------
## PCA
##-------------------------------------------------------------------------------------------------
# Take a random subset of the training data to test the different PCA:

n<-nrow(training_set)
##preparing some data
idx <- seq(1:n)
idx <- idx[sample(1:n)]
##shuffel the data
data<-training_set[idx[1:5000],] ## PCA analysis can't study the 50000 points, so we just take a subset

##training linear kernel function.........................................................

pca.linear<-prcomp(~.,data=data[,-ncol(data)])
pca.linear.variance<-pca.linear$sdev^2/sum(pca.linear$sdev^2)
pca.linear.cum.variance<-pca.linear.variance[1]
for(i in 2:length(pca.linear.variance)){
  pca.linear.cum.variance[i]<-pca.linear.variance[i]+pca.linear.cum.variance[i-1]
}


##training polynomial kernel function....................................................

pca.poly<-kpca(~.,data=data[,-ncol(data)],kernel="polydot",kpar=list(degree=2)) 
pca.poly.variance<-as.numeric(eig(pca.poly)/sum(eig(pca.poly)))
pca.poly.cum.variance<-pca.poly.variance[1]
for(i in 2:length(pca.poly.variance)){
  pca.poly.cum.variance[i]<-pca.poly.variance[i]+pca.poly.cum.variance[i-1]
}



##-------------------------------------------------------------------------------------------------
## Results
##-------------------------------------------------------------------------------------------------


## 1. plot the variance explained by the tree methods
##-------------------------------------------------------------------------------------------------

exp.variance<-c(pca.linear.cum.variance[1:10],pca.poly.cum.variance[1:10])
exp.variance<-cbind(rep(seq(1,10,1),2),exp.variance)
exp.variance<-as.data.frame(exp.variance)
exp.variance$type<-c(rep("linear",10),rep("polynomial",10))
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

vectors<-predict(pca.poly,training_set[,-ncol(training_set)])
vectors<-as.data.frame(vectors)
vectors<-vectors[,1:30]


test.vectors<-predict(pca.poly,testing_set)
vectorssel<-vectors[,1:2]
vectorssel<-as.data.frame(vectorssel)
vectorssel$type<-data$Cover_Type
svm.poly<-svm(type~.,data=vectorssel, kernel="radial", scale=FALSE,
              cost=10, gamma=0.5)
ypredict<-predict(svm.poly,test.vectors[,1:2])



error<-c()
for(i in 1:20){ ## loop to see the evolution of the training error, as we include more PC
  vectorssel<-vectors[,1:i]
  vectorssel<-as.data.frame(vectorssel)
  vectorssel$type<-data$Cover_Type
  svm.poly<-svm(type~.,data=vectorssel, kernel="radial", scale=FALSE,
      cost=10, gamma=0.5)
  ypredict<-predict(svm.poly) 
  error[i]<-mean(vectorssel$type!=ypredict)
  cat("i",i)
  cat("error",error[i])
}

svm<-svm(Cover_Type~.,data=training_set, kernel="radial", scale=FALSE,
    cost=10, gamma=0.5)
Radial_ypredict<-predict(svm) 
error.svm<-mean(training_set$Cover_Type!=Radial_ypredict)

error<-cbind(seq(1,20,1),errors)
error.svm<-(cbind(seq(1,20,1),rep(error.svm,20)))
error<-rbind(error,error.svm)
error<-as.data.frame(error)
colnames(error)<-c("PC","error")
method<-c(rep("PCA",20),rep("NON-PCA",20))
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


