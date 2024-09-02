##--- Sacha Medjo-Akono 
##--- Problem Set 1 Fall_2024
rm(list=ls())
mydata <- read.csv("Data/Protozoa.csv")
View(mydata)
##-- Question 2
MeanTop<-mean(mydata$Top, na.rm=TRUE)
MeanBottom<-mean(mydata$Bottom)
MeanTop
MeanBottom 
##
SDTop<-sd(mydata$Top, na.rm=TRUE)
SDBottom<-sd(mydata$Bottom)
SDTop
SDBottom
##
VarianceTop<-var(mydata$Top, na.rm=TRUE)
VarianceBottom<-var(mydata$Bottom)
##
CITop<-t.test(mydata$Top, na.rm=TRUE)
CIBottom<-t.test(mydata$Bottom)
##-- Note added to doc can't tell how to make this online one number? 