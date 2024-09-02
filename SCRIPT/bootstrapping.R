###How to bootstrap and jackknife to determine confidence in our estimates
# Created by Casey terHorst, revised by Jeremy Yoder
# 28 Aug 2024

#Let's use the Snail Data from the Intro to R script again.

rm(list=ls()) #clear the environment


#Load the data. 
mydata <- read.csv("Data/SnailData.csv")


#Let's say we want to know the mean of our snail lengths. It's easy to get our 
#sample mean
mean(mydata$Length)

#But how certain are we that this mean represents the population mean
#well, one way is to resample the mean of our sample population over and over,
#leaving one data point out each time

bootmeans<-replicate(1000, { #this tells R I want it to do the same thing 1000 times; open brackets start a function  
  samples<-sample(mydata$Length,replace=TRUE); #this will take a subsample of our original data, with replacement every subsample
  mean(samples)  }) #take the mean of the subsample

#Now you have 1000 different estimates of the mean, based on your 1000 random samples
#You can see them by typing
bootmeans

#We can just take the mean of our bootstrapped means to get
mean(bootmeans) #the more bootstrapped samples we use, the closer to the population mean we should get

#You could calculate the confidence interval just as we have before.
#With bootstraps, it's nice because if we sort our samples in order, then exactly one of your values represents the upper and lower 95% confidence limit

#To sort your bootstrapped means:
sortedboots<-sort(bootmeans)

#Now our 1000 means are sorted in order. If we'd done 1000 bootstraps, then the 25th value would be the lower CL and the 975th sample would be the lower CL
lowCI<-sortedboots[25]
highCI<-sortedboots[975]
lowCI
highCI

#If you want to know the actual confidence interval, then you can calculate the 
#difference between the mean and the confidence limit
upperCI<-highCI - mean(bootmeans)
lowerCI<-mean(bootmeans) - lowCI
upperCI
lowerCI

#You could also make a histogram of your bootstrapped means if you want
hist(sortedboots)

#To add vertical lines for the two confidence limits
abline(v=lowCI, col="blue")
abline(v=highCI, col="blue")
#The above command says, put a vertical line (v) at the value of the low CI and make the line blue
#Note that if you had calculated the 95% CI, this value would be mean - 95% CI. And the 
#upper value would be mean + 95% CI.


#If we had a low sample size, we might want to jackknife instead of bootstrap:
library(resample)
jmean<- jackknife(mydata$Length, mean) #first argument is the data to be jackknifed. Second argument is the statistic you want to calculate
jmean
