#--Sacha Medjo-Akono
#--Problem Set 5 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
#--Question 1
library("pwr")
#Pwr analysis--a
1/1.5 #mean group 1 mins mean group too is same as diff of 1g
pwr.t.test(d=0.667, sig.level=0.05, power=0.80, type="two.sample")
#Pwr analysis--b
3/1.5
pwr.t.test(d=2, sig.level=0.05, power=0.80, type="two.sample")
#Pwr analysis--c
pwr.t.test(d=2, sig.level=0.01, power=0.80, type="two.sample")
#Pwr analysis--d
pwr.t.test(n=10, sig.level=0.05, power=0.80, type="two.sample") #d=1.324947
(1.324947 *1.5)
#--Question 2
rm(list=ls())
Herb <- read_csv("Homeworks/PS5/Medicago.csv")
View(Herb)
#
qqp(Herb$Fruits, "norm")
qqp(Herb$Fruits, "lnorm") #WOW...
library("MASS")
gamma <- fitdistr(Herb$Fruits, "gamma")
qqp(Herb$Fruits, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]]) #best
#Very similar to gamma
nbinom <- fitdistr(Herb$Fruits, "Negative Binomial")
qqp(Herb$Fruits, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) #similar to gamma
#
poisson <- fitdistr(Herb$Fruits, "Poisson")
qqp(Herb$Fruits, "pois", lambda=poisson$estimate) #chaotic
#Models and AICs
Herbmod1 <- glm(Fruits~Disturbance*Range, family=gaussian(link="identity"), data=Herb)
Herbmod2 <- glm(Fruits~Disturbance*Range, family=gaussian(link="log"), data=Herb)
Herbmod3 <- glm(Fruits~Disturbance*Range, family=Gamma(link="inverse"), data=Herb)
AIC(Herbmod1)
AIC(Herbmod2)
AIC(Herbmod3) #Best Model
#Anova shows 
Anova(Herbmod3, type="III")
#Random genotype nested within range
library(lme4)
Herbmod4 <- glmer(Fruits~Disturbance + Range + Disturbance:Range + (1|Genotype) +
                    (1|Disturbance:Genotype) + (1|Range:Genotype) + (1|Disturbance:Range:Genotype),
                  data=Herb) #refuse to use * so changed to :
names(Herb)
Anova(Herbmod4, type="III")
#
Herbmod5 <- glmer(Fruits~Disturbance + Range + Disturbance:Range + (1|Genotype),
                  family=Gamma(link="inverse"), data=Herb) #Best AIC
Herbmod6 <- glm(Fruits~Disturbance + Range + Disturbance:Range,
                  family=Gamma(link="inverse"), data=Herb) #No genotype
anova(Herbmod4, Herbmod5)
anova(Herbmod5, Herbmod6)
#
Anova(Herbmod5, type="III")
#Plot
library(MoMAColors)
display.all.moma()
ggplot(Herb, aes(y=Fruits, x=Range, fill=Range)) + 
  geom_boxplot() + 
  scale_fill_manual(values=moma.colors("Warhol"), labels=c("Invasive", "Native"),
                             name="Range") + 
  facet_wrap("Disturbance") + scale_y_log10() + theme_minimal() + 
  labs(y="Fruit production", x="Range", title="Medicago Genotype Affects Fruit Production in Response to \nDisturbance Within Collection Ranges") +
  theme(legend.position="bottom")
#--Question 3

