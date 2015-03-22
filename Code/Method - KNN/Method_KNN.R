##-------------------------------------------------------------------------------------------------
##   KNN
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
#  this code is going to study the classification throught KNN method


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
if (!require("e1071")) install.packages("e1071")
if (!require("class")) install.packages("class")##Knn 

if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
rm(list=ls())

noCores <- makeCluster(detectCores()) ## detect the cores in the machine

source("Code/ReadData.R")

##-------------------------------------------------------------------------------------------------
##   End of "preparing" the data, now the training beggins
##-------------------------------------------------------------------------------------------------


data<-training_set


##-------------------------------------------------------------------------------------------------
##   KNN
##-------------------------------------------------------------------------------------------------

set.seed(4321)

digits<-data[,-ncol(data)]
digits$Y<-data[,ncol(data)]

error<-matrix(0,10,3)
error<-as.data.frame(error)
noObs<-nrow(data)
ks<-c(1,2,3)

for (i in 1:10){
  # bucket indicator
  noBuckets <- 2
  idx <- rep(1:noBuckets, each=ceiling(noObs/noBuckets))  
  # each bucket of size 600
  
  # you never know how the data was constructed, so we randomize the buckets
  # just in case
  idx <- idx[sample(1:noObs)]
  idx <- idx[1:noObs]  # if it is an odd number
  
  # adding the variable
  digits <- digits %>% mutate(bucketId=idx)
  
  # Running in parallel
  
  # to go over both iterators simultaneously, they need to be of equal length
  bucketList <- rep(1:noBuckets, length(ks))
  kList <- rep(ks, noBuckets)
  
  cl <- makeCluster(noCores, type="SOCK", outfile="") 
  registerDoSNOW(cl)
  
  system.time(
    results<- foreach(bucket = bucketList, k = kList, 
                      .combine=rbind, .packages=c("class", "dplyr")) %dopar% {
  
                 
                        # some helpful debugging messages
                        cat("Bucket", bucket, "is the current test set! k=", k, "\n")
                        
                        # subsetting the training phase data
                        Xtrain <- digits %>% filter(bucketId != bucket) %>% select(-bucket, -Y)
                        Ytrain <- digits %>% filter(bucketId != bucket) %>% select(Y)
                        
                        # subsetting the test phase data
                        Xtest <- digits %>% filter(bucketId == bucket) %>% select(-bucket, -Y)
                        Ytest <- digits %>% filter(bucketId == bucket) %>% select(Y)
                        
                        # kNN results
                        testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain$Y, k=k)
                        testError <- mean(testPredictions != Ytest$Y)
                        
                        # last thing is returned
                        result <- c(bucket, k, testError)
                      }
  )
  
  stopCluster(cl)
  
  
  # get the CV error
  colnames(results) <- c("testBucket", "k", "Error")
  er<-as.data.frame(results) %>% group_by(k) %>% summarize(cvError=mean(Error))
  error[i,]<-t(er[,2])
  
}

ind<-seq(1,110000,10000)
KNN_results<-c()

for(i in 1:(length(ind)-1)){
  testPredictions <- knn(train=training_set[,-ncol(training_set)], test=testing_set[ind[i]:(ind[i+1]-1),], cl=training_set$Cover_Type, k=1)
  KNN_results<-c(KNN_results,testPredictions)
}

KNN_results<-KNN_results[,2]
##  -----------------------------------------------------------------------------
## Save the results
##  -----------------------------------------------------------------------------
KNN_results<-cbind(id_testing,KNN_results)

KNN_results <- data.frame(id =  id_testing, Cover_Type = KNN_results)

write.table(KNN_results, "KNN_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(Linear_results, "Linear_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)


##  -----------------------------------------------------------------------------


