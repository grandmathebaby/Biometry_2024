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
#
