#--Sacha Medjo-Akono
#--Problem Set 3 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(MoMAColors)
library(lmodel2)
#-1
rm(list=ls())
krat <- read_csv("Homeworks/PS3/krat.csv")
View(krat)
density <- krat$krat_density
home <- krat$shrubcover
snek <- krat$snakedensity
food <- krat$seedproduction
#Loaded Model 2 because Density is random.
ratmod1 <- lm()