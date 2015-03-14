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
testing_trueLabel<-read.table("Data/Kaggle_Covertype_sample.csv", sep=",",header=T)
id_testing <- testing_set$id  ## keep the id 
training_set <- training_set[,-1]  ## remove the id column
testing_set  <- testing_set[,-1]

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
##-------------------------------------------------------------------------------------------------
## Approach with different weights

## Radial -----------------------------------------------------------------------------
#cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)
Radial_costList<-c(0.1,1,10)
gammaList<-c(0.5,1,2)
Radial_costList<-rep(Radial_costList,length(gammaList))
Radial_gammaList<-c(rep(gammaList[1],3),rep(gammaList[2],3),rep(gammaList[3],3))



registerDoParallel(cl)


results<- foreach(ct = Radial_costList, ga = Radial_gammaList, 
                  .combine=rbind, .packages=c("class", "dplyr","e1071")) %dopar% {
                    
                    Radial_svm<-svm(Class~.,data=data[1:10000,],scale=F,
                                    class.weights = c(a=0.1,b=0.1,c=10,d=10,e=10,f=10,g=10),
                           kernel="radial",cost=ct,gamma=ga)
                    
                    Radial_ypredict<-predict(Radial_svm,testing_set[1:1000,]) ##predict labels (best model)
                    Radial_error<-mean(testlabel[1:1000]!=Radial_ypredict) ## error of Radial
                    
                    result <- c(ga, ct, Radial_error)
                  }
                    
stopCluster(cl)


a<-ksvm(Class~.,data=data[1:10000,],C=10, kernel="polydot",scale=F,
        class.weights = c(a=100,b=100,c=1,d=1,e=2,f=2,g=2))
Radial_ypredict<-predict(a,testing_set[1:1000,]) 
mean(testlabel[1:1000]!=Radial_ypredict)
table(Radial_ypredict,testlabel[1:1000])

##trying an other library that uses majority vote

library(sklearn)##not available


### let's do 1 againts 1 in each step

dataA<-subset(data, Class=="a")
dataB<-subset(data, Class=="b")
dataC<-subset(data, Class=="c")
dataD<-subset(data, Class=="d")
dataE<-subset(data, Class=="e")
dataF<-subset(data, Class=="f")
dataG<-subset(data, Class=="g")

na<-round(nrow(dataA)*0.1)
nb<-round(nrow(dataB)*0.1)
nc<-round(nrow(dataC)*0.3)
nd<-round(nrow(dataD)*0.5)
ne<-round(nrow(dataE)*0.5)
nf<-round(nrow(dataF)*0.35)
ng<-round(nrow(dataG)*0.35)

dataAB<-rbind(dataA[sample(1:na),],dataB[sample(1:nb),])
dataAC<-rbind(dataA[sample(1:na),],dataC[sample(1:nc),])
dataAD<-rbind(dataA[sample(1:na),],dataD[sample(1:nd),])
dataAE<-rbind(dataA[sample(1:na),],dataE[sample(1:ne),])
dataAF<-rbind(dataA[sample(1:na),],dataF[sample(1:nf),])
dataAG<-rbind(dataA[sample(1:na),],dataG[sample(1:ng),])
dataBC<-rbind(dataB[sample(1:nb),],dataC[sample(1:nc),])
dataBD<-rbind(dataB[sample(1:nb),],dataD[sample(1:nd),])
dataBE<-rbind(dataB[sample(1:nb),],dataE[sample(1:ne),])
dataBF<-rbind(dataB[sample(1:nb),],dataF[sample(1:nf),])
dataBG<-rbind(dataB[sample(1:nb),],dataG[sample(1:ng),])
dataCD<-rbind(dataC[sample(1:nc),],dataD[sample(1:nd),])
dataCE<-rbind(dataC[sample(1:nc),],dataE[sample(1:ne),])
dataCF<-rbind(dataC[sample(1:nc),],dataF[sample(1:nf),])
dataCG<-rbind(dataC[sample(1:nc),],dataG[sample(1:ng),])
dataDE<-rbind(dataD[sample(1:nd),],dataE[sample(1:ne),])
dataDF<-rbind(dataD[sample(1:nd),],dataF[sample(1:nf),])
dataDG<-rbind(dataD[sample(1:nd),],dataG[sample(1:ng),])
dataEF<-rbind(dataE[sample(1:ne),],dataF[sample(1:nf),])
dataEG<-rbind(dataE[sample(1:ne),],dataG[sample(1:ng),])
dataFG<-rbind(dataF[sample(1:nf),],dataG[sample(1:ng),])


AB<-svm(Class~.,data=dataAB,cost=2, kernel="radial",scale=FALSE)
AC<-svm(Class~.,data=dataAC,cost=2, kernel="radial",scale=FALSE)
AD<-svm(Class~.,data=dataAD,cost=2, kernel="radial",scale=FALSE)
AE<-svm(Class~.,data=dataAE,cost=2, kernel="radial",scale=FALSE)
AF<-svm(Class~.,data=dataAF,cost=2, kernel="radial",scale=FALSE)
AG<-svm(Class~.,data=dataAG,cost=2, kernel="radial",scale=FALSE)

BC<-svm(Class~.,data=dataBC,cost=2, kernel="radial",scale=FALSE)
BD<-svm(Class~.,data=dataBD,cost=2, kernel="radial",scale=FALSE)
BE<-svm(Class~.,data=dataBE,cost=2, kernel="radial",scale=FALSE)
BF<-svm(Class~.,data=dataBF,cost=2, kernel="radial",scale=FALSE)
BG<-svm(Class~.,data=dataBG,cost=2, kernel="radial",scale=FALSE)

CD<-svm(Class~.,data=dataCD,cost=2, kernel="radial",scale=FALSE)
CE<-svm(Class~.,data=dataCE,cost=2, kernel="radial",scale=FALSE)
CF<-svm(Class~.,data=dataCF,cost=2, kernel="radial",scale=FALSE)
CG<-svm(Class~.,data=dataCG,cost=2, kernel="radial",scale=FALSE)

DE<-svm(Class~.,data=dataDE,cost=2, kernel="radial",scale=FALSE)
DF<-svm(Class~.,data=dataDF,cost=2, kernel="radial",scale=FALSE)
DG<-svm(Class~.,data=dataDG,cost=2, kernel="radial",scale=FALSE)

EF<-svm(Class~.,data=dataEF,cost=2, kernel="radial",scale=FALSE)
EG<-svm(Class~.,data=dataEG,cost=2, kernel="radial",scale=FALSE)

FG<-svm(Class~.,data=dataFG,cost=2, kernel="radial",scale=FALSE)

##predict
id<-seq(1,nrow(testing_set),1)
ypredict<-data.frame(id)


ypredict$AB<-predict(AB,testing_set)
ypredict$AC<-predict(AC,testing_set)
ypredict$AD<-predict(AD,testing_set)
ypredict$AE<-predict(AE,testing_set)
ypredict$AF<-predict(AF,testing_set)
ypredict$AG<-predict(AG,testing_set)

ypredict$BC<-predict(BC,testing_set)
ypredict$BD<-predict(BD,testing_set)
ypredict$BE<-predict(BE,testing_set)
ypredict$BF<-predict(BF,testing_set)
ypredict$BG<-predict(BG,testing_set)

ypredict$CD<-predict(CD,testing_set)
ypredict$CE<-predict(CE,testing_set)
ypredict$CF<-predict(CF,testing_set)
ypredict$CG<-predict(CG,testing_set)

ypredict$DE<-predict(DE,testing_set)
ypredict$DF<-predict(DF,testing_set)
ypredict$DG<-predict(DG,testing_set)

ypredict$EF<-predict(EF,testing_set)
ypredict$EG<-predict(EG,testing_set)

ypredict$FG<-predict(FG,testing_set)

Mode <- function(x) {
  mode<-as.numeric(names(table(x))[which.max(table(x))])
  return(mode)
}

ypredict$mode<-rep(0,nrow(ypredict))
for(i in 1:nrow(ypredict)){
  ypredict$mode[i]<-Mode(as.numeric(ypredict[i,-1]))
}



a<-ksvm(Class~.,data=dataTraing,C=2, kernel="rbfdot",scale=FALSE)

Radial_ypredict<-predict(a,dataTest[,-ncol(dataTest)]) 
mean(dataTest[,ncol(dataTest)]!=Radial_ypredict)
table(Radial_ypredict,dataTest[,ncol(dataTest)])

mean(ypredict$mode!=test1label)

table(ypredict$mode,test1label)


results

ga<-0.1
ct<-100
tune(svm,Class~.,data=data[1:10000,],scale=F,
     class.weights = c(a=0.1,b=0.1,c=10,d=10,e=10,f=10,g=10),range=list(cost=c(0.1,1,10),gamma<-c(0.5,1,2)))


## Polynomial -----------------------------------------------------------------------------

Radial_costList<-c(0.1,1,10)
degree<-c(2,3,4)

cost<-Radial_costList[2]
degree<-4
registerDoParallel(cl)

Poly_svm<-tune(svm,Cover_Type~.,data=data, kernel="polynomial", scale=FALSE,
               range=list(cost=c(0.1,1,10),degree=c(0.5,1,2,3,4)))

Poly_ypredict<-predict(Poly_svm,testing_set) ##predict labels (best model)

Poly_error<-mean(testing_trueLabel[,2]!=Poly_ypredict) ## error of Radial

Radial_result <- c(cost, gamma, Radial_error)


stopCluster(cl)

##  -----------------------------------------------------------------------------
## Save the results
##  -----------------------------------------------------------------------------

write.table(Radial_results, "Radial_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(Linear_results, "Linear_Results.csv", sep = ",", row.names = FALSE, quote = FALSE)


##  -----------------------------------------------------------------------------


