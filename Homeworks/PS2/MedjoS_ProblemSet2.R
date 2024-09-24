#--Sacha Medjo-Akono
#--Problem Set 2 - Fall 2024
rm(list=ls())
urchins <- read_csv("Homeworks/PS2/Urchins.csv")
View(urchins)
##load packages
library(tidyverse)
library(car)
library(psych)
library(moments)
##--1a
UFlat <- urchins  %>%
  filter(Location == "Flat")
UComplex <- urchins %>%
  filter(Location == "Complex")
##
flatmean <- mean(UFlat$Density)
complexmean <- mean(UComplex$Density)
##--
Varflat<-var(UFlat$Density)
Varcomplex<-var(UComplex$Density)
##--
SDflat<-sqrt(Varflat)
SDcomplex<-sqrt(Varcomplex)
##--1b
UPooled <- t.test(Density~Location, var.equal=TRUE, data=urchins)
UPooled
##
USep <- t.test(Density~Location, data=urchins)
USep
##--1c
ggplot(urchins, aes(y=Density, x=Location, fill=Location)) + 
  geom_boxplot(alpha=0.5) +
  geom_jitter(width=0.2) + 
  labs(
    title = "Increased Urchin Density on Rocks with Complex Topography",
    x = "Rock Topography",
    y = "Urchin Density"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("maroon", "dodgerblue")) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )
##--2
rm(list=ls())
Wheat <- read_csv("Homeworks/PS2/CoastBuckwheat.csv")
View(Wheat)
#--Data is normal
shapiro.test(Wheat$Density)
##--One tailed t-test because we only have one eg of constant
Wheatest <- t.test(Wheat$Density, mu=4, na.rm=TRUE)
Wheatest
#--Graph
SortedWheat<-sort(Wheat$Density)
SortedWheat
##-- Because sample is 30 I used the CI derived from t test results
lowCI <- 4.366935
highCI <- 6.766398
lowCI
highCI
##--FIXING THIS
ggplot(Wheat, aes(x=Wheat$Density)) +
  geom_histogram() +
  labs(x="Quadrat", y="Wheat Density", title="Coastal Buckwheat Density Mean Exceeds Standard After Restoration") +
  theme(legend.position="bottom") +
  geom_vline(xintercept = lowCI, col="blue") +
  geom_vline(xintercept = highCI, col="blue") +
  geom_hline(yintercept = 4)

##-- With Bootstrapping
WheatBoots<-replicate(1000, {
  samples<-sample(mydata$Length,replace=TRUE);
  mean(samples)  })