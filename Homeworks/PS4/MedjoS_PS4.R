#--Sacha Medjo-Akono
#--Problem Set 4 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
#--Question 1
rm(list=ls())
magnet <- read_csv("Homeworks/PS4/electromagnetic_effects.csv")
View(magnet)
#With random effect BEST MODEL
library(lme4)
magnetized <- lmer(WBCcolonies~Treatment + (1|Donor), data=magnet)
plot(magnetized)
summary(magnetized)
anova(magnetized)
#No random
magnetized2 <- lm(WBCcolonies~Treatment, data=magnet)
plot(magnetized2)
anova(magnetized2)
anova(magnetized,magnetized2)
#Both Random
magnetized3 <- lmer(WBCcolonies~(1|Treatment) + (1|Donor), data=magnet)
summary(magnetized3)
#--Question 2
rm(list=ls())
plant <- read_csv("Homeworks/PS4/plantcompetition.csv")
View(plant)
#Making factors
plant$Species <- as.factor(plant$Species)
plant$clipped <- as.factor(plant$clipped)
plant$weeded <- as.factor(plant$weeded)
#Non-normal flower count
qqnorm(plant$flowers)
qqline(plant$flowers) 
hist(plant$flowers)
#
skewness(plant$flowers)
kurtosis(plant$flowers)
#Tried CLT but still not trusting it
samp200 <- vector(mode="numeric", length=1000)

for(r in 1:1000){
  samp200[r] <- mean(sample(plant$flowers, 200)) 
}

ggplot(data.frame(flowers=samp200), aes(x=flowers)) + 
  geom_histogram(fill="#fb9a99") + theme_bw()
skewness(samp200)
kurtosis(samp200)
#Square-root transformation
squareflower <- sqrt(plant$flowers)
squareflower
#Three-way ANOVA
library(lme4)
install.packages("lmerTest")
library(lmerTest)
#Model w Species Random + Interactions
plantmod <- lmer(squareflower~clipped + weeded + (1|Species) + clipped*weeded +
                   (1|Species:weeded) + (1|Species:clipped) + (1|Species:clipped:weeded), data=plant)
summary(plantmod)
anova(plantmod)
#No Random + No interactions
plantmod2 <- lm(squareflower~clipped + weeded + clipped*weeded, data=plant)
anova(plantmod, plantmod2)
#Species Random + No interactions
plantmod3 <- lmer(squareflower~clipped + weeded + + clipped*weeded + (1|Species), data=plant)
anova(plantmod, plantmod2, plantmod3)
#Plot
library(emmeans)
graphdata <- as.data.frame(emmeans(plantmod, ~clipped + weeded + + clipped*weeded))
graphdata
#Labels
clipped.labs <- c("Clipped Yes", "Clipped No")
names(clipped.labs) <- c("yes", "no")
#
ggplot(graphdata, aes(x=weeded, y=emmean, fill=as.factor(weeded))) + 
  geom_bar(stat="identity", position="dodge", linewidth=0.6) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE),
                stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  labs(x="weeded", y="flower production") + 
  facet_wrap("clipped", labeller = labeller(clipped = clipped.labs)) +
  scale_fill_manual(values=c("yes"="seagreen","no"="salmon"), name="weeded") +  
  theme_bw(base_size=14)
#--Question 3
rm(list=ls())
coraline <- read_csv("Homeworks/PS4/coral_acclimation.csv")
View(coraline)
#Making factors
coraline$temperature <- as.factor(coraline$temperature)
coraline$coral <- as.factor(coraline$coral)
coraline$day <- as.factor(coraline$day)
#Looks normal
qqp(coraline$FvFm, "norm")
#Model w random coral + interaction between temp/day
library(lme4)
library(lmerTest)
coralmod <- lmer(FvFm ~ temperature + day + temperature:day + (1|temperature/coral), data=coraline)
anova(coralmod)
#Plot
library(emmeans)
library(MoMAColors)
display.all.moma()
coralgraph <- as.data.frame(emmeans(coralmod, ~temperature:day))
coralgraph
#
ggplot(coralgraph, aes(x=day, y=emmean, group=temperature, color=temperature)) +
  geom_line() +
  geom_point(size=2)+
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", width=0.1) +
  scale_color_manual(values=moma.colors("Warhol"), labels=c("Ambient", "Warm", "Hot"),
                     breaks=c("ambient", "warm", "hot")) +
  labs(x="Day", y="FvFm", color="Temperature", title="Coral Acclimation is Dependent on Time and Temperature") + 
  theme_minimal()
#--Question 4
rm(list=ls())
weeds <- read_csv("Homeworks/PS4/stipe_strength.csv")
View(weeds)
#Making Factors
weeds$species <- as.factor(weeds$species)
#Normality
weedmod1 <- lm(BreakForce ~ species * Thickness, data=weeds)
#plot(weedmod1) #Cone-shaped, will log transform
thicklog <- log(weeds$Thickness)
brokenlog <- log(weeds$BreakForce)
#Log model with interaction of thickness and species
weedmod2 <- lm(brokenlog~species * thicklog, data=weeds)
#plot(weedmod2)
anova(weedmod2)
#Log model no interaction
weedmod3 <- lm(brokenlog~thicklog, data=weeds)
anova(weedmod3)
plot(weedmod3)
#
AIC(weedmod2) #Chosen because we care about interaction 17.78381
AIC(weedmod3) #Better 17.34563
# We have 28 Ccrispus and 37 Mstellatus so model is unbalanced
library(car)
Anova(weedmod2, type="III")
#Plot
breakingbad <- predict(weedmod2) 
#
weedgraph <- cbind(weeds, breakingbad) 
# We'll plot the original data as points, and the regression preditions
# as lines --- and in this case you can see that the relationship between
# length and weight is very very similar at the two tidal heights
ggplot(data=weedgraph, aes(x=thicklog, y=brokenlog, color=species)) +
  geom_point() + 
  geom_line(aes(y=breakingbad)) +
  labs(x="Log Thickness", y="Log Breakforce", color="Species",
       title="Tissue Thickness Accounts for Likelihood of Seaweed\nDislodgment by Storm Waves") +
  scale_color_manual(values=moma.colors("Liu"), labels=c("C. crispus", "M. Stellatus")) +
  theme_minimal() 
#--Sacha Medjo-Akono
#--Problem Set 4 - Fall 2024