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
anova(magnetized)
#No random
magnetized2 <- lm(WBCcolonies~Treatment, data=magnet)
plot(magnetized2)
anova(magnetized2)
anova(magnetized,magnetized2)
#Both Random
magnetized3 <- lmer(WBCcolonies~(1|Treatment) + (1|Donor), data=magnet)
summary(magnetized3)
#