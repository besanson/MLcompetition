# MLcompetition
BGSE ML course competition
Team Alessio
Team members:
* Maria
* Jordan
* Gaston

  
**Index**:
The following folder contains all the used codes to test different methods and create the report. The structure of the folder is the following:

	-Code: contains the following files and folders:
		- Packages: creates a function to install and load libraries
		- ReadData: reads data into R and creates the new features
		- Graph: Code for creating some Graphs for the report
		- Folders: each folder contains the code to test each of the models
		
	-Data: contains the initial training and setting data
	-Report: 
		-RMD of the report
		-Graphs: folders of all the final graph in png format
		-Results: error of the training set with different parameters and methods
´
**Instructions**:  
You can open this repository as a *R Studio Project* that automatically sets the `MLCompetition folder` as default (please, if you decide to do other approach remember to set this folder as the working directory).
The code to test our final GBM choice is located at: `Code-> Mehod-GBM -> R code`.  
**Please remember that the H2o package runs on `Java Virtual Machine` , it uses the `RJava` package. If you are using the AWS machine with the Rstudio image you have to to install `JVM` and `RJava` from the command line**.
