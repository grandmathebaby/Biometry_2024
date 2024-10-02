##--- Sacha Medjo-Akono 
##--- Problem Set 1 Fall_2024
rm(list=ls())
Protozoa<- read.csv("Data/Protozoa.csv")
View(Protozoa)
##load packages
library(tidyverse)
library(car)
library(psych)
library(moments)
#Getting the stats
MeanTop<-mean(Protozoa$Top)
MeanTop
MeanBottom<-mean(Protozoa$Bottom)
MeanBottom
##--
VarTop<-var(Protozoa$Top)
VarTop
VarBottom<-var(Protozoa$Bottom)
VarBottom
##--
SDTop<-sqrt(VarTop)
SDTop
SDBottom<-sqrt(VarBottom)
SDBottom
##--
CITop<-MeanTop-qt(0.95,9)*SDTop
CIBottom<-MeanBottom+qt(0.95,9)*SDBottom
##-- Question 2 -- Plot
SETop<-SDTop / sqrt(length(Protozoa$Top))
SETop
SEBottom<-SDBottom / sqrt(length(Protozoa$Bottom))
SEBottom
##--
ProtoSummary<-data.frame(
  group = c("Top", "Bottom"),
  mean = c(MeanTop, MeanBottom),
  SE = c(SETop, SEBottom)
)
##--
ggplot(ProtoSummary, aes(group, mean, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.2) +
  labs(
    title = "Increased Protozoa Density at the Bottom of Microcosm Indicates a Higher Food Availability",
    x = "Protozoa Location",
    y = "Protozoa Density (per uL)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("maroon", "orange")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )
##-- Question 3
rm(list=ls())
Gonad<- read.csv("Data/kelp bass gonad mass.csv")
View(Gonad)
##--
Mass<-Gonad$gonad_mass
##--
MeanGonad<-mean(Mass)
MedianGonad<-median(Mass)
VarianceGonad<-var(Mass)
SDGonad<-sd(Mass)
CVGonad<-(SDGonad/MeanGonad)
##--
SkewGonad<-skewness(Mass)
KurtosisGonad<-kurtosis(Mass)
##--3d
hist(Mass)
##--3e
ZMass<-scale(Gonad$gonad_mass, center=TRUE, scale=TRUE)
hist(ZMass)
##--3f
qqnorm(Gonad$gonad_mass)
qqline(Gonad$gonad_mass)
##--Question 6
rm(list=ls())
Coral<- read.csv("Data/Agaricia.csv")
View(Coral)
##--Observing Skewness and hist of original data
SkewCoral<-skewness(Coral$weight)
hist(Coral$weight)
#--Converting to Z Scores and hist of zscore data
ZCoral<-scale(Coral$weight, center=TRUE, scale=TRUE)
hist(ZCoral)
##--Normality of original
qqnorm(Coral$weight)
qqline(Coral$weight)
##--Normality of logged
qqnorm(log10(Coral$weight))
qqline(log10(Coral$weight))
##--6b
CoralBoots<-replicate(1000, { 
  samples<-sample(Coral$weight,replace=TRUE);
  mean(samples)  })
mean(CoralBoots)

SortedCoral<-sort(CoralBoots)

lowCI<-SortedCoral[25]
highCI<-SortedCoral[975]
lowCI
highCI
##--
upperCI<-highCI - mean(CoralBoots)
lowerCI<-mean(CoralBoots) - lowCI
upperCI
lowerCI
##--
hist(SortedCoral)
abline(v=lowCI, col="blue")
abline(v=highCI, col="blue")