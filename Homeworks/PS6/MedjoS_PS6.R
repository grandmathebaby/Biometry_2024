#--Sacha Medjo-Akono
#--Problem Set 6 - Fall 2024
#--Sacha Medjo-Akono
#--Problem Set 5 - Fall 2024
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
fishy <- read_csv("Homeworks/PS6/BahamasFish.csv")
View(fishy)
#
fish <- fishy[,-1] #Remove the site column
fishzscore <- scale(fish, scale=TRUE, center=TRUE) #z-scores
PCAmodel <- princomp(fishzscore, cor=FALSE)
summary(PCAmodel) 
#
