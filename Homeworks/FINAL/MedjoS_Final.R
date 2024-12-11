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
#--Question 2
#There are 214 dead animals
rm(list=ls())
rural <- 119
urban <- 95
#
observed <- matrix(c(rural, urban), nrow=1, ncol=2)
expected <- matrix(c((rural+urban)/2, (rural+urban)/2), nrow=1, ncol=2)
#G Value
Gvalue <- 2*sum(observed*log(observed/expected))
Gvalue #is 2.69726
1-pchisq(Gvalue, df=1)
#p-value = 0.1005209
#Chi-squared test
X2test <- sum((observed-expected)^2/expected)
X2test # is 2.691589
1-pchisq(X2test, df=1) #p-value = 0.01008792
#--Question 3



