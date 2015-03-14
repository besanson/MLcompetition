##-------------------------------------------------------------------------------------------------
##   Graph
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to create some data set and graph for the report

## USEFULL LINKS:


## GENERAL INFO:


## Explanetion of the created variables/tables/functions

## Colours.tree....... colour palette selected for all the graphs that make reference to the tree type


##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Machine Learning/MLcompetition")


## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")
if (!require("scatterplot3d")) install.packages("scatterplot3d")## 3D graphs


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
##   Colours
##-------------------------------------------------------------------------------------------------

Colours.tree<-c("#d53e4f","#fc8d59","#fee08b","#ffffbf","#e6f598","#99d594","#3288bd")



##-------------------------------------------------------------------------------------------------
##   Type of Trees Graph
##-------------------------------------------------------------------------------------------------

## graph showing the portion of the trees in our training set

training_set$Cover_Type_label<-factor(training_set$Cover_Type, levels=c(1,2,3,4,5,6,7),
                                      labels=c("Spruce","Lodgepole Pine","Ponderosa Pine",
                                               "Cottonwood/Willow","Aspen","Douglas-fir","Krummholz"))


ggplot(training_set, aes(x=Cover_Type_label,fill=Cover_Type_label))+geom_bar()+
  scale_fill_manual(name="colour",values=Colours.tree)+
  theme(axis.text.x = element_text(angle=-30),panel.background = element_rect(fill = "white"))+
  labs(title="Distribution of the type of Trees",x="Type of tree")
  

  
##Export Results:     ----- . ----- . ----- . ----- . ----- . ----- . ----- .
png(filename="Report/Graphs/Type_Tree", width = 1100, height = 700)
ggplot(training_set, aes(x=Cover_Type_label,fill=Cover_Type_label))+geom_bar()+
  scale_fill_manual(name="colour",values=Colours.tree)+
  theme(axis.text.x = element_text(angle=-30),panel.background = element_rect(fill = "white"))+
  labs(title="Distribution of the type of Trees",x="Type of tree")
dev.off()
## End of Exporting   ----- . ----- . ----- . ----- . ----- . ----- . ----- .


##-------------------------------------------------------------------------------------------------
##   3D representation of some of the variables
##-------------------------------------------------------------------------------------------------

training_set$colour<-rep(0,nrow(training_set))
for(i in 1:nrow(training_set)){
  ind<-training_set$Cover_Type[i]
  training_set$colour[i]<-Colours.tree[ind]
}

##thinking of doing animated by changin the angle, so it turns 
with(training_set,scatterplot3d(elevation, hor_dist_fire, hor_dist_road,
                        color = colour, pch = 20,angle = 70,
                        col.grid = NULL, col.axis = "grey",))

png(filename="Report/Graphs/3D_plot", width = 1100, height = 700)
with(training_set,scatterplot3d(elevation, hor_dist_fire, hor_dist_road,
                                color = colour, pch = 20,angle = 70,
                                col.grid = NULL, col.axis = "grey",))
dev.off()
