#Examining normality
# Casey terHorst, revised by Jeremy Yoder
# 28 Aug 2024

#Let's use the Snail Data

#Clear the environment
rm(list=ls())

#Load the data
mydata <- read.csv("Data/SnailData.csv")

#to get skewness and kurtosis, I use the moments package
library(moments)
skewness(mydata$Length)
kurtosis(mydata$Length)

#to make a histogram:
hist(mydata$Length)

#to convert to z scores
zscoreLength<-scale(mydata$Length, center=TRUE, scale=TRUE) 
#Center centers the data on the mean (subtracts mean); Scale divides by s.d.
hist(zscoreLength)

#to get probability plot
qqnorm(mydata$Length)
qqline(mydata$Length)

#To make a probability plot with confidence intervals
library(car)

qqp(mydata$Length, "norm")

qqp(mydata$Weight, "norm")
