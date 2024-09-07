##--- Sacha Medjo-Akono 
##--- Problem Set 1 Fall_2024
rm(list=ls())
Protozoa<- read.csv("Data/Protozoa.csv")
View(Protozoa)
##load packages
library(tidyverse)
library(car)
library(psych)
#Getting the stats
MeanTop<-mean(Protozoa$Top)
MeanTop
MeanBottom<-mean(Protozoa$Bottom)
MeanBottom
##--
SDTop<-sd(Protozoa$Top)
SDBottom<-(Protozoa$Top)
##--
Variance<-var(Protozoa$Density, na.rm = TRUE)
Var

VarianceBottom<-var(Protozoa$Density)
##--
CITop<-MeanTop-qt(0.95,19)*SDTop
CIBottom<-MeanBottom+qt(0.95,19)*SDBottom
##-- Question 2

##Making my dataset to work with


ggplot(Protozoa, aes(MeanTop, MeanTop)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) + 
  geom_col(position=dodge) +
  geom_errorbar(aes(ymin=MeanTop-SDTop, ymax=MeanTop+SDTop), width=0.2)

##--???
##-- Question 3
rm(list=ls())
Gonad<- read.csv("Data/kelp bass gonad mass.csv")
View(Gonad)
##--
MeanGonad<-mean(Gonad$gonad_mass)
MedianGonad<-median(Gonad$gonad_mass)
VarianceGonad<-var(Gonad$gonad_mass)
SDGonad<-sd(Gonad$gonad_mass)
CVGonad<-(SDGonad/MeanGonad)
##--
## install.packages("PerformanceAnalytics")
## library(PerformanceAnalytics)
##--
SkewGonad<-skewness(Gonad$gonad_mass)
KurtosisGonad<-kurtosis(Gonad$gonad_mass)