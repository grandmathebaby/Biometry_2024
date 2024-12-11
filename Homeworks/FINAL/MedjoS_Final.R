#--Sacha Medjo-Akono
#--Final Exam - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
library(lme4)
library(pscl)
library(factoextra)
library("pwr")
library(MASS)
library(MoMAColors)
display.all.moma()

#--Question 1
rm(list=ls())
froggy <- read_csv("Homeworks/FINAL/treefrog.csv")
View(froggy)
#Normality ok
qqnorm(froggy$SVL)
qqline(froggy$SVL)
qqnorm(froggy$weight)
qqline(froggy$weight)
#
frogmod <- lm(weight~SVL, data=froggy)
frogmod
#
frogres <- resid(frogmod)
qqp(frogres, "norm")
#
plot(frogres~fitted(frogmod)) #not a cone :)
summary(frogmod)
