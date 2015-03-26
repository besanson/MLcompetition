##-------------------------------------------------------------------------------------------------
##   Graph
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to create some data set and graph for the report

## USEFULL LINKS:


## GENERAL INFO:


## Expl. of the created variables/functions

## Colours.tree....... colour palette selected for all the graphs that make reference to the tree type
## Types of Tree...... bar plot with the portions of the different trees
## 3D graph........... graph that shows the 3d representation of 3 of the attributes


###List of Graphs

##plot1............... Bar plot of the number of each Type of Trees in our daya
##plot2............... Line plot with the variance capture by PCA

##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Machine Learning/MLcompetition")

rm(list=ls())

## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")
if (!require("doMC")) install.packages("doMC")
if (!require("doParallel")) install.packages("doParallel")
library(gridExtra)

if (!require("scatterplot3d")) install.packages("scatterplot3d")## 3D graphs
##-------------------------------------------------------------------------------------------------
##   Data Files
##-------------------------------------------------------------------------------------------------

source("Code/ReadData.R") ## all variables
PCA_Var<-read.csv("Data/tables/PCA_Variance.csv",sep=";")

##-------------------------------------------------------------------------------------------------
##   Colours
##-------------------------------------------------------------------------------------------------

Colours.tree<-c("#d53e4f","#fc8d59","#fee08b","#ffffbf","#e6f598","#99d594","#3288bd")

##-------------------------------------------------------------------------------------------------
##   Density and Boxplot Graph
##-------------------------------------------------------------------------------------------------

BoxplotData<-as.data.frame(cbind(or.training_set$elevation,rep("training",nrow(or.training_set))))
BoxplotData<-rbind(BoxplotData,cbind(or.testing_set$elevation,rep("testing",nrow(or.testing_set))))
colnames(BoxplotData)<-c("elevation","data")
BoxplotData$elevation<-as.numeric(BoxplotData$elevation)


box.plot1<-ggplot(BoxplotData,aes(factor(data),elevation))+geom_boxplot(aes(fill=factor(data)))+
  theme(axis.text.x = element_text(angle=-30),panel.background = element_rect(fill = "white"),
        legend.position="none")+
  scale_fill_manual(name="colour",values=Colours.tree[c(1,7)])+
  labs(title="BoxPlot of Elevation",x="data")

DensityData<-as.data.frame(cbind(or.training_set$slope,rep("training",nrow(or.training_set))))
DensityData<-rbind(DensityData,cbind(or.testing_set$slope,rep("testing",nrow(or.testing_set))))
colnames(DensityData)<-c("slope","data")
DensityData$slope<-as.numeric(DensityData$slope)



density<-ggplot(DensityData,aes(x=slope,fill=factor(data))) + 
  geom_density(alpha=0.3)+
  scale_fill_manual(name="colour",values=Colours.tree[c(1,7)])+
  theme(axis.text.x = element_text(angle=-30),panel.background = element_rect(fill = "white"))+
  scale_y_continuous(breaks=c(0.01,0.02,0.03,0.04,0.05))+
  labs(title="Density of Slope",x="data")


rm(BoxplotData)
rm(DensityData)

##-------------------------------------------------------------------------------------------------
##   Type of Trees Graph
##-------------------------------------------------------------------------------------------------

## graph showing the portion of the trees in our training set

training_set$Cover_Type_label<-factor(training_set$Cover_Type, levels=c(1,2,3,4,5,6,7),
                                      labels=c("Spruce","Lodgepole Pine","Ponderosa Pine",
                                               "Cottonwood/Willow","Aspen","Douglas-fir","Krummholz"))


##Export Results:     ----- . ----- . ----- . ----- . ----- . ----- . ----- .
plot1<-ggplot(training_set, aes(x=Cover_Type_label,fill=Cover_Type_label))+geom_bar()+
  scale_fill_manual(name="colour",values=Colours.tree)+
  theme(axis.text.x = element_text(angle=-30),panel.background = element_rect(fill = "white"))+
  labs(title="Distribution of the type of Trees",x="Type of tree") 
## End of Exporting   ----- . ----- . ----- . ----- . ----- . ----- . ----- .


##-------------------------------------------------------------------------------------------------
##   3D representation of some of the variables
##-------------------------------------------------------------------------------------------------

training_set$colour<-rep(0,nrow(training_set))
for(i in 1:nrow(training_set)){
  ind<-training_set$Cover_Type[i]
  training_set$colour[i]<-Colours.tree[ind]
}



##-------------------------------------------------------------------------------------------------
##   PCA Explained Variance
##-------------------------------------------------------------------------------------------------

plot2<-ggplot(PCA_Var,aes(x=PC,y=Variance,colour=Method))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(1,10,1))+ylab("Explained Variance")


