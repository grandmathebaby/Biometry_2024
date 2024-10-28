#--Sacha Medjo-Akono
#--Midterm 2 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)

#--Question 1
rm(list=ls())
marisa <- read_csv("Homeworks/Midterm2/redbull.csv")
View(marisa)
#No Random Effect
class(marisa$Treatment)
marisa1 <- lm(testscore ~ Treatment, data = marisa)
plot(marisa1)
#Data is normally distributed
resmarisa <- residuals(marisa1)
qqp(resmarisa, "norm")
#
anova(marisa1)
#
library("emmeans")
emmeans(marisa1, pairwise~"Treatment", adjust="Tukey")
#
library("agricolae")
HSD.test(marisa1, "Treatment", console=TRUE)
#plot
library(MoMAColors)
display.all.moma()
marisaplot <- as.data.frame(emmeans(marisa1, "Treatment"))
marisaplot
#
marisaplot$tukey <- HSD.test(marisa1, "Treatment", console=FALSE)$groups$groups  
#
ggplot(marisaplot, aes(x=Treatment, y=emmean, fill=Treatment)) +
  geom_bar(width=0.5, stat="identity") + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), color="black", stat="identity",
                position=position_dodge(width=0.9), width=0.1) +
  guides(fill="none") +
  geom_text(aes(label=tukey), vjust=-1.5) +
  scale_fill_manual(values=moma.colors("Klein"), labels=c("Redbull", "Coke", "Water")) +
  labs(x="Treatment", y="Test Score", title="Sugar and Caffeine Consumption Before Exam \nAffect Test Scores") + 
  theme_minimal()

#--Question 2
rm(list=ls())
tina <- read_csv("Homeworks/Midterm2/gobyshelter.csv")
View(tina)
#Making factors
tina$Predators <- as.factor(tina$Predators)
tina$Shelter <- as.factor(tina$Shelter)
tina$Block <- as.factor(tina$Block) #depth
colnames(tina)
class(tina$`Gobitat #`)#R refuses to use pound sign

#Model with interactions and main effects no Habitat number
tinamod1 <- lm(`goby recruits`~Block + Shelter + Predators + Block:Shelter + Block:Predators + Predators:Shelter, data=tina)
anova(tinamod1)
#Model with interactions and main effects and Habitat number
tinamod2 <- lm(`goby recruits`~Block*Shelter + Block*Predators + Predators*Shelter + `Gobitat #`*Block + `Gobitat #`*Shelter +
                 `Gobitat #`*Predators, data=tina)
anova(tinamod2)
#plot(tinamod2)

tinamod3 <- lm(`goby recruits`~Block*Shelter + Block*Predators + Predators*Shelter + Predators:Shelter:Block, data=tina)

tinamod4 <- lm(`goby recruits`~Block*Shelter*Predators, data=tina)
anova(tinamod4)

tinamod5 <- lm(`goby recruits`~Block*Predators*Shelter, data=tina)
anova(tinamod5)

anova(tinamod3)
#plot(tinamod3)

#Shorter Models
tinamod7 <- lm(`goby recruits`~Block*Shelter, data=tina)
tinamod8 <- lm(`goby recruits`~Block*Predators, data=tina)
tinamod9 <- lm(`goby recruits`~Predators*Shelter, data=tina)
tinamod10 <- lm(`goby recruits`~Block, data=tina)
tinamod11 <- lm(`goby recruits`~Shelter, data=tina)
tinamod12 <-  lm(`goby recruits`~Predators, data=tina)
#
AIC(tinamod1) #best aic but uses habitat number not helpful
AIC(tinamod2)
AIC(tinamod3) #best using this
AIC(tinamod4) #same as best but not full mdodel?
AIC(tinamod7)
AIC(tinamod8)
AIC(tinamod9)
AIC(tinamod10)
AIC(tinamod11)
AIC(tinamod12)
#
anova(tinamod3)
#--QUestion 3 
rm(list=ls())
octoocta <- read_csv("Homeworks/Midterm2/octocorals.csv")
View(octoocta)
#
octoocta$Treatment <- as.factor(octoocta$Treatment)
#Model with interaction
octo <- lm(Photosynthesis ~ ColonyDiameter*Treatment, data = octoocta) #Best model
plot(octo)
#Data is normally distributed
resocto <- residuals(octo)
qqp(resocto, "norm")
#
anova(octo)
#Model no interaction
octo1 <- lm(Photosynthesis ~ ColonyDiameter + Treatment, data=octoocta)
anova(octo1)
#AICs
install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(octo,octo1)
aictab(cand.set=models, modnames=c("octo", "octo1"))
#Best model
anova(octo)
#plot
octopredict <- predict(octo) #did not accept the regular as.data.frame, have to bind this to data
octograph <- cbind(octoocta, octopredict)
#
ggplot(octograph, aes(x=ColonyDiameter, y=Photosynthesis, color=Treatment)) +
  geom_point(size=2, stat="identity") + 
  geom_line(aes(y=octopredict)) +
  scale_color_manual(values=moma.colors("Smith"), labels=c("Control", "Removal")) +
  labs(x="Stony Coral Diameter (cm)", y="Photosyntheic Rate", title="Presence of Octocorals and Diameter of Stony Coral \nAffect Photosynthetic Rate of Stony Coral") + 
  theme_minimal()

#--Question 4
rm(list=ls())
rumpus <- read_csv("Homeworks/Midterm2/bumpus.csv")
View(rumpus)
#Making factors
rumpus$Survival <- as.factor(rumpus$Survival)
rumpus$Sex <- as.factor(rumpus$Sex)
#Normally Distributed
rumpus1 <- lm(Weight ~ Length * Sex * Survival, data=rumpus)
summary(rumpus1)
anova(rumpus1)
Anova(rumpus1, type="III")
qqp(rumpus1, "norm")
#Models with no interaction
rumpus2 <- lm(Weight ~ Length + Sex + Survival, data=rumpus) #Best with all factors
rumpus3 <- lm(Weight ~ Sex + Survival, data=rumpus)
rumpus4 <- lm(Weight ~ Length + Sex, data=rumpus)
rumpus5 <- lm(Weight ~ Length + Survival, data=rumpus) 
#Solo models
rumpus6 <- lm(Weight ~ Length, data=rumpus) #Best model but doesn't answer question
rumpus7 <- lm(Weight ~ Sex, data=rumpus)
rumpus8 <- lm(Weight ~ Survival, data=rumpus)
#Models with interaction
rumpus9 <- lm(Weight ~ Sex*Survival, data=rumpus)
rumpus10 <- lm(Weight ~ Length*Sex, data=rumpus)
rumpus11 <-  lm(Weight ~ Length*Survival, data=rumpus) #Best with interaction
#
library(AICcmodavg)
models <- list(rumpus1, rumpus2, rumpus3, rumpus4, rumpus5, rumpus6, rumpus7, rumpus8, rumpus9, 
               rumpus10, rumpus11)
aictab(cand.set=models, modnames=c("rumpus1", "rumpus2", "rumpus3", "rumpus4", "rumpus5", "rumpus6",
                                   "rumpus7", "rumpus8", "rumpus9", "rumpus10", "rumpus11"))
#Using best model with all factors
Anova(rumpus2, type=("III")) #unbalanced data use type 3
#
rumpuspredict <- predict(rumpus2)
rumpgraph <- cbind(rumpus, rumpuspredict)
#Plot
Survival.labs <- c("Survived", "Died")
names(Survival.labs) <- c("TRUE", "FALSE")

ggplot(data=rumpgraph, aes(x=Length, y=Weight, color=Sex, shape=Sex)) +
  geom_point(size=2) + geom_line(aes(y=rumpuspredict)) +
  facet_wrap("Survival", labeller = as_labeller(c("TRUE" = "Survived", "FALSE" = "Died"))) +
  scale_color_manual(values=moma.colors("Smith"), labels=c("f", "m")) +
  labs(x="Length (cm)", y="Weight (g)", fill="Sex", title= "Length Predicts Weight and Survival of Sparrows") +
  theme_minimal()
#--Sacha Medjo-Akono
#--Midterm 2 - Fall 2024