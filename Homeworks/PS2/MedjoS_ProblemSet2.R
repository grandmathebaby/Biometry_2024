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
##--1
flatmean <- mean(urchins$Flat)
complexmean <- mean(urchins$Complex)
##--
Varflat<-var(urchins$Flat)
Varcomplex<-var(urchins$Complex)
##--
SDflat<-sqrt(Varflat)
SDcomplex<-sqrt(Varcomplex)