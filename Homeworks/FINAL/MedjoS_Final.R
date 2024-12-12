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
1-pchisq(X2test, df=1) #p-value = 0.1008792
#--Question 3
rm(list=ls())
ana <- read_csv("Homeworks/FINAL/anorexia.csv")
View(ana)
#
mean(ana$WeightChange)
median(ana$WeightChange)
sd(ana$WeightChange)
sd(ana$WeightChange)/sqrt(length(ana$WeightChange))
sd(ana$WeightChange)/mean(ana$WeightChange)*100
qqp(ana$WeightChange, "norm")
#Unequal variances
anacon <- ana %>%
  filter(Treatment=="Control")
anagone <- ana %>%
  filter(Treatment=="Therapy")
varcon <- var(anacon$WeightChange)
vargone <- var(anagone$WeightChange)
#Welch's test
anattest <- t.test(WeightChange~Treatment, data=ana)
anattest
#Plot
library(ggsignif)
ggplot(ana, aes(y=WeightChange, x=Treatment, fill=Treatment)) + 
  geom_boxplot(alpha=0.8) +
  geom_jitter(width=0.2) + 
  labs(
    title = "CBT Improves Weight Gain in Patients with Anorexia",
    x = "Treatment",
    y = "Weight Change (kg)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = moma.colors("Althoff", 2)) +
  theme(
    plot.title = element_text(hjust = 0.5)) +
  geom_signif(data=ana,stat="signif",position="identity",
              comparisons=list(c("Control","Therapy")),map_signif_level = TRUE,annotations="***")
#--Question 4
rm(list=ls())
Bass <- read_csv("Homeworks/FINAL/habitats.csv")
View(Bass)
#
Bass1 <- Bass[,-c(1,2,3)] #Only 6 habitat traits
bass.scale <- scale(Bass1, scale=TRUE, center=TRUE) #convert to z-scores
# 
PCAmodel <- princomp(bass.scale, cor=FALSE)
summary(PCAmodel)
#Scree Plot
plot(PCAmodel, type="lines")
library(factoextra)
fviz_eig(PCAmodel)
