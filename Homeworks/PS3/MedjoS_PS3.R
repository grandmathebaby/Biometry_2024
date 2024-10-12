#--Sacha Medjo-Akono
#--Problem Set 3 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
#-1
rm(list=ls())
krat <- read_csv("Homeworks/PS3/krat.csv")
View(krat)
rats <- krat$krat_density
home <- krat$shrubcover
snek <- krat$snakedensity
food <- krat$seedproduction
#
qqnorm(rats)
qqnorm(home)
qqnorm(snek)
qqnorm(food)
#Loaded Model 2 because Density is not fixed by Camdilla
#Separate models
rathome1 <- lmodel2(rats~home, range.y="relative", range.x="relative", data=krat, nperm=99)
rathome1
summary(rathome1)
ggplot(krat, aes(x=rats, y=home))+
  geom_smooth(method = "lm", formula = y~x, color="white") +
  geom_point(color="darkorange") + # points
  labs(y = "Shrub Cover", x="Rat Density") +
  theme_bw(base_size=18)
#
ratfood1 <- lmodel2(rats~food, range.y="relative", range.x="relative", data=krat, nperm=99)
ratfood1
summary(ratfood1)
ggplot(krat, aes(x=rats, y=food))+
  geom_smooth(method = "lm", formula = y~x, color="white") +
  geom_point(color="blue") + # points
  labs(y = "Food", x="Rat Density") +
  theme_bw(base_size=18)
#
ratdead1 <- lmodel2(rats~snek, range.y="relative", range.x="relative", data=krat, nperm=99)
ratdead1
summary(ratdead1)
ggplot(krat, aes(x=rats, y=snek))+
  geom_smooth(method = "lm", formula = y~x, color="white") +
  geom_point(color="darkgreen") + # points
  labs(y = "Predation", x="Rat Density") +
  theme_bw(base_size=18)
#--Multiple regression
OGrats <- lm(rats ~ food + home + snek, data=krat)
library(GGally)
X <- krat[,c("shrubcover","seedproduction","snakedensity")] # just the columns named
ggpairs(X)