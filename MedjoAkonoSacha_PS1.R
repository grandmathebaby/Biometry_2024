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
##
CITop<-t.test(Protozoa$Top, na.rm=TRUE)
CIBottom<-t.test(Protozoa$Bottom)

##-- Note added to doc can't tell how to make this online one number? 
##-- Question 3
rm(list=ls())
Gonad<- read.csv("Data/kelp bass gonad mass.csv")
View(Gonad)
##
MeanGonad<-mean(Gonad$gonad_mass)
MedianGonad<-median(Gonad$gonad_mass)
VarianceGonad<-var(Gonad$gonad_mass)
SDGonad<-sd(Gonad$gonad_mass)
CVGonad<-(SDGonad/MeanGonad)
##
## install.packages("PerformanceAnalytics")
## library(PerformanceAnalytics)
##
SkewGonad<-skewness(Gonad$gonad_mass)
KurtosisGonad<-kurtosis(Gonad$gonad_mass)
