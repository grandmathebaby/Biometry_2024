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
plot(model1) #cone shaped not normal
var.test(ratio, nitro)
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
  scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086','skyblue')) +
  guides(fill="none") + 
  ylab("Nitrogen Level (N2)") +
  xlab("Root-Shoot Ratio") +
  theme_bw(base_size=14)
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
