##--- Sacha Medjo-Akono 
##--- Problem Set 1 Fall_2024
rm(list=ls())
Protozoa<- read.csv("Data/Protozoa.csv")
View(Protozoa)
##-- Question 2
MeanTop<-mean(Protozoa$Top, na.rm=TRUE)
MeanBottom<-mean(Protozoa$Bottom)
MeanTop
MeanBottom 
##
SDTop<-sd(Protozoa$Top, na.rm=TRUE)
SDBottom<-sd(Protozoa$Bottom)
SDTop
SDBottom
##
VarianceTop<-var(Protozoa$Top, na.rm=TRUE)
VarianceBottom<-var(Protozoa$Bottom)
##--
CITop<-MeanTop-qt(0.95,19)*SDTop
CIBottom<-MeanBottom+qt(0.95,19)*SDBottom
CITop
CIBottom
##load packages
library(tidyverse)
library(car)
##ggplot(Protozoa) + 
#geom_boxplot(aes(x=Top, y=MeanTop)) + 
#scale_fill_manual(values=c("#9bc9fa","maroon"), name="Density") +
#scale_y_log10() + theme_bw() + 
#labs(y="Mean", x="Density", title="Protozoa population density increases with food available") +
#theme(legend.position="none")

ggplot(Protozoa, aes(Top, MeanTop)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) 
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