#--Sacha Medjo-Akono
#--Midterm 1 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(MoMAColors)
#-- 1
rm(list=ls())
cancer <- read_csv("Homeworks/Midterm1/cancer.csv")
View(cancer)
#Looks Normally distributed and var acceptable
shapiro.test(cancer$CellGrowth)
Varcancer<- cancer %>%
  group_by(Time) %>%
  summarize(variance=var(CellGrowth, na.rm=TRUE))
bartlett.test(CellGrowth~Time, data=cancer)
#--Paired t-test
Control <- cancer %>%
  filter(Time == "Baseline")
Treated <- cancer %>%
  filter(Time =="NewDrug")
tcancer <- t.test(Control$CellGrowth, Treated$CellGrowth, paired=TRUE)
tcancer
#--2
rm(list=ls())
warmbodies <- read_csv("Homeworks/Midterm1/temps.csv")
View(warmbodies)
warmth <- warmbodies$Temperature
peeps <- warmbodies$Person
#--a
meanwarmth <- mean(warmth)
medianwarmth <- median(warmth)
sdwarmth <- sd(warmth)
sewarmth <- sdwarmth/sqrt(length(warmth))
cvwarmth <- (sdwarmth/meanwarmth)
#--b
warmboots <- replicate(1000, {
  warmingup <- sample(warmth, replace=TRUE);
  mean(warmingup) })
#
mean(warmboots)
sortedboots <- sort(warmboots)
lowci <- sortedboots[25]
highci <- sortedboots[975]
#
lowerci <- mean(warmboots)-lowci
upperci <- highci-mean(warmboots)
lowerci
upperci
#
hist(sortedboots)
abline(v=lowci, col="blue")
abline(v=highci, col="blue")
