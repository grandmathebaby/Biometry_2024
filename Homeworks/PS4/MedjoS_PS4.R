#--Sacha Medjo-Akono
#--Problem Set 4 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
#--Question 1
rm(list=ls())
magnet <- read_csv("Homeworks/PS4/electromagnetic_effects.csv")
View(magnet)
#With random effect BEST MODEL
library(lme4)
magnetized <- lmer(WBCcolonies~Treatment + (1|Donor), data=magnet)
plot(magnetized)
summary(magnetized)
anova(magnetized)
#No random
magnetized2 <- lm(WBCcolonies~Treatment, data=magnet)
plot(magnetized2)
anova(magnetized2)
anova(magnetized,magnetized2)
#Both Random
magnetized3 <- lmer(WBCcolonies~(1|Treatment) + (1|Donor), data=magnet)
summary(magnetized3)
#--Question 2
rm(list=ls())
plant <- read_csv("Homeworks/PS4/plantcompetition.csv")
View(plant)
#Making factors
plant$Species <- as.factor(plant$Species)
plant$clipped <- as.factor(plant$clipped)
plant$weeded <- as.factor(plant$weeded)
#Normality of flower count
qqnorm(plant$flowers)
qqline(plant$flowers) 
hist(plant$flowers)
#Not normally distributed
logflowers <- log(plant$flowers)
logflowers
hist(logflowers) #not happy in the model
sqrtflowers <- sqrt(plant$flowers)
sqrtflowers
hist(sqrtflowers)
qqnorm(sqrtflowers)
qqline(sqrtflowers)


#Three-way ANOVA
library(lme4)
install.packages("lmerTest")
library(lmerTest)
#Model w Species Random
plantmod <- lmer(sqrtflowers~clipped + weeded + (1|Species) + clipped*weeded +
                   (1|Species:weeded) + (1|Species:clipped) + (1|Species:clipped:weeded), data=plant)
summary(plantmod)
anova(plantmod)
