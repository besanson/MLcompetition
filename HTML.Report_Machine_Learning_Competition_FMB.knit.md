---
title: 'Machine Learning Competition: Report on Forest Cover Type Prediction'
author: "Fernandez, María, McIver, Jordan, Besanson, Gaston"
output: html_document
---

```{r,echo=FALSE, message=FALSE}

setwd("C:/Users/DON/Dropbox/ML Competition/MLcompetition")
source("Code/Graph.R")## the Graphs will be written in the Graph code, so here we only have to call them
```
# Introduction

\newthought{This report} has the purpose to explain our experience on the Forest Cover Type Prediction's Competition.^[https://inclass.kaggle.com/c/prediction-of-a-forest-covertype] This experience starts with `Data exploration and Feature Creation`, `Pre-Processing`, `Machine Learning Methods`, `Results over this dataset`, `Conclusion`.

Our main objetive for this project was to predicte as best as possible the type of tree in a region with the purpose of minimizing the fires. Even though that we used the same loss for each type of missclassification -in other words, all trees are equally important-, we decided to create new features^[The recommended approach would be consider penalizing more the missclassification of very flamable trees]. **This feature is explained in detailed in the attached powerpoint presentation**.

# Data Description

The dataset used in this report consists of 54 attributes (or features) and 50.000 observations for the training (plus one class attribute) and 100.000 observations for the competition.
The attributes are

* 10 quantitative data:  
  • Elevation in range 1859–3858 (m).
  • Aspect (azimuth from true north) in range 0–360 (azimuth).
  • Slope in range 0–66 (◦).
  • Horizontal Distance To Hydrology in range 0–1397 (m).
  • Vertical Distance To Hydrology in range −173–601 (m).
  • Horizontal Distance To Roadways in range 0–7117 (m).
  • Hillshade 9 a.m. in range 0–255 (index).
  • Hillshade Noon in range 0–255 (index).
  • Hillshade 3 p.m. in range 0–255 (index).
  • Horizontal Distance To Fire Points in range 0–7173 (m).
*  4 binary wilderness areas and 40 binary soil type variables.

The trees are classify in 7 different types: 

First, we checked that our training set was representative enought of the testing data.  The means and quantiles of each feature  where very similar betweeen the two data sets. That gave us a hint, that the model in the training set performs as good as the testing set; *dismissing our initial concerns of overfitting*. The following boxplot shows the similarity of `Elevation` of the two data sets. Besides, a density plot of the `Slope` shows that the two data sets are equally distributed. **QUOTE SAYING THAT THESE ARE THE MOST IMPORTANT ONES. and therefore our training set is good proxy**

```{r,echo=FALSE}

pushViewport(viewport(layout = grid.layout(2, 5)))
print(density, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:3)) 
print(box.plot1, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 3:5))

```

One of the first things noticed in the data is that the portion of trees are very irregular. The following graph shows the portions of each type of tree in our data. We can see that mainly we have two type of trees *Spruce* and *Lodgepole Pine*, after those two types the other have a smaller presence. We will have to take this into account to check how our classifications methods work with the trees with less observations.

```{r,echo=FALSE}
plot1

```

First step before starting to "play" with the data has to scale the variables so they would all be *normalize*. We have to destinguish two type of data, the binary and the continous. The first ones should not be included in the normalization.

#are we sure?


The next thing we wanted to consider was creating some more informative variables in the data

##JORDAN YOU SHOULD EXPLAIN THIS!

**The Graph represent 3 variables that where already in the data set, and the second plot gives how our transformations might help classifying. I'm not sure they are helpful, will need to decide.**

```{r,echo=FALSE}


with(training_set,scatterplot3d(elevation, hor_dist_fire, hor_dist_road,
                                       color = colour, pch = 20,angle = 70, 
                                       col.grid = NULL, col.axis = "grey"))



with(training_set,scatterplot3d(fire_risk, horDistHyd_ElevShift, crowFliesHyd,
                                color = colour, pch = 20,angle = 90, 
                                col.grid = NULL, col.axis = "grey"))

```


Once we have done all the preparation of the data we have left with 59 variables that we are going to use for in the different classification methods.

# Pre-Processing

As we created new variables and still are keeping the old ones...
We wanted to try reducing dimensiality in order to decrees the run-time of our different classification models. However, the loss of variance would mean decreasing the models performance. We study the two main methods use in PCA with the linear and polynomial transformation of the data.
One of the main things we have to take into account when doing PCA, is the portion of information we loose. As we are trying to minimize the classification errror of our different models, we want to make sure we don't loose any valuable information doing some pre-processing or PCA. The following graph shows the portion of explained variance as we take more principal components. That means the accumulated information that each principal component includes. We should be interested in a transformation that with few principal components includes as maximum of variance as possible. If we take a transformation with less that 90% of the information, that could reduce our classification accurancy.

We can see in the Graph that the only method that keeps most of the variance is the polynomial transformation. 

```{r, fig.width = 7, fig.height = 3, fig.cap = "PCA"}
plot2
```

Creating the vectors with the polynomial transformation may capture 90% of the information, but will it help explain the Cover Type of our data? To see how much the missclassification error might increase we check with SVM method the difference in accurancy. The following graph shows firstly the error with SVM with all training data. On the other hand, it shows how the classifation error decreases as we include a new principal component. As we can see in the following graph to get a similar performance in the training error with the PCA we would need 30 components, and even then the classification is not stabel. So from this PCA study we decide to proceed without using PCA.

```{r, fig.width = 7, fig.height = 3, fig.cap = "PCA"}


PCA.error<-read.table("Data/tables/PCA_error.csv",sep=";")

ggplot(PCA.error,aes(x=PC,y=error,colour=type))+geom_line()+theme_bw()+
  scale_x_continuous(breaks=seq(0,30,5))

```



# Machine Learning Methods

From the theory classes and previous works in the same data base, we decided we where going to study the performance of Random Forest, Support Vector Machine, Gradient Boosting Methods and Deep Learning.

## Random Forest
What package we used. Why we select this method. Literature

## Support Vector Machine
Support Vector Machine is a algorithm that tries to find a hyperplane that splits the data with the maximum margin. When data is perfect separable we try to find a hard margin classifier that correctly classifies all the data points. When the data is not perfectly separable we introduce a slack variable and a cost related to the misclassification. In our case, the data is not perfectly separable so we introduce a cost for missclassification, further more, it can not be separated by a linear function. To solve this we introduce the kernel transformations and try classify the points. 

## Gradient Boosting Methods
What package we used. Why we select this method. Literature

## Deep Learning. Just for fun
What package we used. Why we select this method. Literature

# Results

Decide to do Cross Validation. The accuracy in the training. accuarcy in the leaderboard. ROC. Tables
IDEAS?

If we have the 3 methods.. we can do a voting......

# Conclusion


----------------------------------------------------------------

\begin{marginfigure}
$$\frac{d}{dx}\left( \int_{0}^{x} f(u)\,du\right)=f(x).$$
\caption{An equation}
\end{marginfigure}

Note the use of the `\caption` command to add additional text below the equation.

## Full Width Figures

You can arrange for figures to span across the entire page by using the `fig.fullwidth` chunk option. 

```{r, fig.width = 10, fig.height = 2, fig.fullwidth = TRUE, fig.cap = "Full width figure"}
library(ggplot2)
qplot(wt, mpg, data=mtcars, colour=factor(cyl))
```















