#--Sacha Medjo-Akono
#--Problem Set 3 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
#--Question 1
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
OGrats
#Check for collinearity
library(GGally)
X <- krat[,c("shrubcover","seedproduction","snakedensity")] # just the columns named
ggpairs(X)
library(mctest)
imcdiag(OGrats) #multicollinearity for food
#
ratsnohome <- lm(rats ~ snek + food)
hungryrats <- lm(rats ~ snek + home)
saferats <- lm(rats ~ home + food)
rathome2 <- lm(rats ~ home)
ratfood2 <- lm(rats ~ food)
ratdead2 <- lm(rats ~ snek)
#--Best 1 to Worst 7
AIC(OGrats) # 2
AIC(ratsnohome) # 5
AIC(hungryrats) # Winner 1
AIC(saferats) # 4
AIC(rathome2) # 3
AIC(ratfood2) # 6
AIC(ratdead2) # 7
#
install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(OGrats, ratsnohome, hungryrats, saferats, rathome2, ratfood2, ratdead2)
model.names <- c("OGrats", "ratsnohome", "hungryrats", "saferats", "rathome2", "ratfood2", "ratdead2")
aictab(cand.set=models, modnames=model.names)
#
anova(OGrats)
anova(hungryrats)
#
library(rockchalk)
plotPlane(OGrats, plotx1="home", plotx2="food")
#--Question 3
rm(list=ls())
rooted <- read_csv("Homeworks/PS3/rootshoot.csv")
View(rooted)
#
nitro <- rooted$Nitrogen
ratio <- rooted$RootShoot
model1 <- lm(ratio~nitro)
model1
#plot(model1)
#logged data conforms to assumptions
Uprooted <- lm(log(ratio)~nitro)
Uprooted
plot(Uprooted)
#ANOVA on logged data
anova(Uprooted)
#Tukey
library("emmeans")
emmeans(Uprooted, pairwise~"nitro", adjust="Tukey")
TukeyHSD(aov(Uprooted))
library(agricolae)
HSD.test(Uprooted, "nitro", console=TRUE) 
#Plot
rootplot <- as.data.frame(emmeans(Uprooted,"nitro") )
rootplot
rootplot$tukey <- HSD.test(Uprooted, "nitro", console=TRUE)$groups$groups
#
ggplot(data=rootplot, aes(x=nitro, y=emmean, fill=nitro)) +
  geom_bar(width=0.5, stat="identity") + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), color="black", stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  geom_text(aes(label=tukey), vjust=-2.5) + # You may need to tweak `vjust` and y limits to make this work
  labs(y = "Root:Shoot Ratio", 
       x="Nitrogen Level (N2)", 
       title="Soil Nitrogen Levels Affect Root-to-shoot Ratio",
       fill= "N2 Level") +
  scale_fill_manual(values=moma.colors("Smith"), labels=c("1.5N", "2N", "Ambient","Low")) +
  theme(legend.position="right") +
  theme_minimal()
#--Question 4
rm(list=ls())
munch <- read_csv("Homeworks/PS3/seedsrodents.csv")
View(munch)
#
munched <- munch$seed.mass
munching <- as.factor(munch$Treatment)
munching
levels(munching)
#-1 Control 
#-2 Exclusion
#-3 Fence control
munch.model <- aov(seed.mass~Treatment, data=munch)
summary(munch.model)
#Creating vectors
#Compare the control vs. fence and exlusion
c1 <- c(1,0,-1)
#Compare control and fence together vs. exclusion
c2 <- c(1,-2,1)
#
mothermunch <- cbind(c1,c2)
mothermunch
munch.model
contrasts(munching) <- mothermunch
summary(munch.model, split=
          list(Treatment=list("Exclusion Effect"=1, "Rodent Effect"=2))
)
#ANOVA
model.munch <- lm(munched~munching)
plot(model.munch)
anova(model.munch)
#Tukey
library("emmeans")
emmeans(model.munch, pairwise~"munching", adjust="Tukey")
TukeyHSD(aov(model.munch))
#--Question 5
rm(list=ls())
coral <- read_csv("Homeworks/PS3/montastraea.csv")
View(coral)
#Random Ramet
library("lme4")
model.coral1 <- lmer(Calyxarea ~ Genotype + (1|Ramet), data=coral)
#plot(model.coral1)
anova(model.coral1)
#Without random effect
model.coral2 <- lm(Calyxarea~Genotype, data=coral)
anova(model.coral1,model.coral2)
#Random genotype and ramet
model.coral3 <- lmer(Calyxarea~(1|Genotype) + (1|Ramet), data=coral)
summary(model.coral3)
#
library("emmeans")
coralplot1 <- as.data.frame(emmeans(model.coral1, ~Genotype))
coralplot1
summary(model.coral1)
#
library(MoMAColors)
ggplot(data=coralplot1, aes(x=Genotype, y=emmean, fill=Genotype)) + 
  geom_bar(width=0.5, stat="identity") + 
  labs(y = "Cakyx Area", x="Genotype", title="M. franksi Calyx Area Variance 57%\n Dependent on Genotype") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), color="black", stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  scale_fill_manual(values=moma.colors("Smith"), labels=c("G1", "G2", "G3")) +
  theme(legend.position="right") +
  theme_minimal()
#--Question 6
rm(list=ls())
crisis <- read_csv("Homeworks/PS3/temp_CO2.csv")
View(crisis)
#
freemepls <- lm(growth~Temperature*CO2, data=crisis)
anova(freemepls)
#plot(freemepls)
#
freethecoral <- as.data.frame(emmeans(freemepls, ~ Temperature*CO2))
freethecoral
#freethecoral$tukey <- HSD.test(freemepls, "Temperature", console=FALSE)$groups$groups  
#freethecoral
#Distinguishing treatments
freethecoral$treatment <- paste0(freethecoral$Temperature," temp")
#
ggplot(freethecoral, aes(x=treatment, y=emmean, fill=CO2)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9), width=0.7) +
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                position=position_dodge(width=0.9), width=0.2) +
  labs(x="Treatment Combination", 
       y="Coral Basal Area Growth (mm²)", 
       fill="pCO2 Level",
       title="Effects of Temperature and pCO2 on Coral Growth Rates") +
  scale_fill_manual(values=moma.colors("Smith"), labels=c("Ambient CO2", "High CO2")) + 
  theme_minimal() +
  theme(legend.position="right")
#--Sacha Medjo-Akono
#--Problem Set 3 - Fall 2024