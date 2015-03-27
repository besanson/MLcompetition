##-------------------------------------------------------------------------------------------------
##   KNN
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
#  this code is going to classify using the KNN method


## USEFULL LINKS:
## https://stat.ethz.ch/R-manual/R-devel/library/class/html/knn.html

## GENERAL INFO:
# We are going to use similar code that was used in the seminar classes, 
# using parallel to find the best value of k

##-------------------------------------------------------------------------------------------------
## LIBRARIES
##-------------------------------------------------------------------------------------------------
# Clear environment
rm(list=ls())
source("Code/Utilities/Packages.R")

###LOAD PACKAGES
loadPackages(c("class","doSNOW","doParallel","dplyr","foreach"))

##-------------------------------------------------------------------------------------------------


##INCLUDE DATA
source("Code/Utilities/ReadData.R")

##-------------------------------------------------------------------------------------------------
##   Training of data
##-------------------------------------------------------------------------------------------------

data<-training_set[,-ncol(training_set)]
data$Y<-training_set[,ncol(training_set)]

##-------------------------------------------------------------------------------------------------
##   KNN method implementation
##-------------------------------------------------------------------------------------------------

set.seed(4321)
noCores <- makeCluster(detectCores()-1) ## detect the cores in the machine


noObs<-nrow(data)
ks<-c(1,2,3) # different options of k

# Bucket indicator
noBuckets <- 10 ## 10 for cross validation of 10 buckets
idx <- rep(1:noBuckets, each=ceiling(noObs/noBuckets))  
# Each bucket is of size 600

# You never know how the data was constructed, 
# so we randomize the buckets just in case
idx <- idx[sample(1:noObs)]
idx <- idx[1:noObs]  # if it is an odd number

# Adding the bucket variable
data <- data %>% mutate(bucketId=idx)

# Running in parallel
bucketList <- rep(1:noBuckets, length(ks))
kList <- rep(ks, noBuckets)

registerDoSNOW(noCores)

# Evaluate test error
results<- foreach(bucket = bucketList, k = kList, 
                    .combine=rbind, .packages=c("class", "dplyr")) %dopar% {
                                         
                      # Subset training phase data
                      Xtrain <- data %>% filter(bucketId != bucket) %>% select(-bucket, -Y)
                      Ytrain <- data %>% filter(bucketId != bucket) %>% select(Y)
                      
                      # Subset test phase data
                      Xtest <- data %>% filter(bucketId == bucket) %>% select(-bucket, -Y)
                      Ytest <- data %>% filter(bucketId == bucket) %>% select(Y)
                      
                      # kNN results
                      testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain$Y, k=k)
                      testError <- mean(testPredictions != Ytest$Y)
                      
                      # Return results
                      result <- c(bucket, k, testError)
                    }

stopCluster(noCores)

# Get the CV error
colnames(results) <- c("testBucket", "k", "Error")
error<-as.data.frame(results) %>% group_by(k) %>% summarize(cvError=mean(Error))

# Error with 1KK is the smaller, although it doesn't peform to good
##  -----------------------------------------------------------------------------
## Save the results with k=1
##  -----------------------------------------------------------------------------
# Have to do a loop because the prediction with knn, can't handle a big set of data
ind<-seq(1,110000,10000)
KNN_results<-c()

for(i in 1:(length(ind)-1)){
  testPredictions <- knn(train=training_set[,-ncol(training_set)], test=testing_set[ind[i]:(ind[i+1]-1),], cl=training_set$Cover_Type, k=1)
  KNN_results<-c(KNN_results,testPredictions)
}


##-------------------------------------------------------------------------------------------------
##   Output the results (different files might be created depending on the parameters selection)
##-------------------------------------------------------------------------------------------------
output <- data.frame(id =  id_testing, Cover_Type = KNN_results)
write.table(output, "Code/Prediction/Pred_Alessio_KNN.csv", sep = ",", row.names = FALSE, quote = FALSE)


