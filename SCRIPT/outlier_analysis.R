#How to identify outliers
#We'll use the SnailData example from the Basic Stats module
#Except I've added an outlier into length, so use this modified dataset instead
#"SnailDataoutlier.csv"
#Clear the environment
rm(list=ls())
#Load the data
mydata <- read.csv("Data/SnailDataoutlier.csv")

#Use a boxplot
boxplot(mydata$Length)

#Look at a graph
plot(Length~Weight, data=mydata)

#Use Bonferroni test to identify
#This only works after you've already fit a model to your data
model1<-lm(Length~Location, data=mydata)
library(car)
outlierTest(model1)
#Tells you whether most extreme value has undue influence
#If P<0.05, then point is a potential outlier

#Can also use Cook's D
influencePlot(model1) #Points greater than 0.1 are potential outliers
#or try
cutoff <- 4/((nrow(mydata)-length(model1$coefficients)-2)) 
plot(model1, which=4, cook.levels=cutoff) 
#Again, values greater than 0.1 are potential outliers
