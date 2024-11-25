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
plot(PCAmodel, type="lines")
#--Question 2
library(factoextra)
fviz_eig(PCAmodel)
#
PCAmodel$loadings #this extracts the vectors
PCAmodel$scores
#
library(devtools)
library(ggbiplot)
ggbiplot(PCAmodel, obs.scale=1, var.scale=1, groups=fishy$Site, ellipse=TRUE, varname.size=3, varname.adjust=1.2, circle=FALSE) +
  scale_color_discrete(name='') +
  geom_point(aes(colour=factor(fishy$Site)), size = 1) + #Color codes by Guild
  theme(legend.direction = 'horizontal', legend.position='bottom', legend.text=element_text(size=8)) +
  theme_bw()
# Another common thing to do with PCA is to make a biplot
biplot(PCAmodel, xlab="PC1", ylab="PC2")
#--Question 3
PCAmodel$loadings #this extracts the vectors
PCAmodel$scores
PC1 <- PCAmodel$scores[,1] # Pulls out the first column of the table 
PC1
#
fishmod <- lm(PC1~fishy$Site)
anova(fishmod)
#--Question 4
