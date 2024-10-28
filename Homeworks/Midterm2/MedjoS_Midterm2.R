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
