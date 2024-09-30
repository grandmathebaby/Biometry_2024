#--Sacha Medjo-Akono
#--Midterm 1 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(MoMAColors)
#-- 1
rm(list=ls())
cancer <- read_csv("Homeworks/Midterm1/cancer.csv")
View(cancer)
#Looks Normally distributed and var acceptable
shapiro.test(cancer$CellGrowth)
Varcancer<- cancer %>%
  group_by(Time) %>%
  summarize(variance=var(CellGrowth, na.rm=TRUE))
bartlett.test(CellGrowth~Time, data=cancer)
#--Paired t-test
Control <- cancer %>%
  filter(Time == "Baseline")
Treated <- cancer %>%
  filter(Time =="NewDrug")
tcancer <- t.test(Control$CellGrowth, Treated$CellGrowth, paired=TRUE)
tcancer
#--2
rm(list=ls())
warmbodies <- read_csv("Homeworks/Midterm1/temps.csv")
View(warmbodies)
warmth <- warmbodies$Temperature
peeps <- warmbodies$Person
#--a
meanwarmth <- mean(warmth)
medianwarmth <- median(warmth)
sdwarmth <- sd(warmth)
sewarmth <- sdwarmth/sqrt(length(warmth))
cvwarmth <- (sdwarmth/meanwarmth)
#--b
warmboots <- replicate(1000, {
  warmingup <- sample(warmth, replace=TRUE);
  mean(warmingup) })
#
mean(warmboots)
sortedboots <- sort(warmboots)
lowci <- sortedboots[25]
highci <- sortedboots[975]
#
lowerci <- mean(warmboots)-lowci
upperci <- highci-mean(warmboots)
lowerci
upperci
#
hist(sortedboots)
abline(v=lowci, col="blue")
abline(v=highci, col="blue")
#--c
#--One sample t-test
warmthtest <- t.test(warmth, mu=98.6)
warmthtest
#--3
rm(list=ls())
kelp <- read_csv("Homeworks/Midterm1/sargassum.csv")
View(kelp)
#--a
#Two sample t-test, using welch's because variances/means may be different in these pops.
Shade <- kelp %>%
  filter(treatment == "underkelp")
Shade
NoShade <- kelp %>%
  filter(treatment =="open")
NoShade
#
kelptest <- t.test(Shade$biomass, NoShade$biomass)
kelptest
#--b
kelpgraph <- kelp %>%
  group_by(treatment) %>%
  summarize(kelpmeans = mean(biomass), SE = sd(biomass)/sqrt(length(biomass)))
kelpgraph
#
ggplot(kelpgraph, aes(x=treatment, y=kelpmeans, fill=factor(treatment), group=factor(treatment))) +
  geom_bar(stat="identity", position="dodge", alpha=1) +
  geom_errorbar(aes(ymin=kelpmeans-SE, ymax=kelpmeans+SE), stat="identity", position="dodge", width = 0.2) +
  theme_minimal() +
  labs(x="Treatment", y="Biomass Mean (g/m^2)", fill="Treatment", title="Invasive Alga S. horneri Mean Biomass Decreases when Shaded by\n Native Alga M. pyrifera") +
  scale_fill_manual(values=moma.colors("Warhol"), labels=c("open", "underkelp"))
#--4
rm(list=ls())
rumpus <- read_csv("Homeworks/Midterm1/bumpus.csv")
View(rumpus)
#--a
qqp(rumpus$Weight, "norm")
qqp(rumpus$Length, "norm")
#--b
#Two-sample t-test with equal variances
bartlett.test(Length~Sex, data=rumpus)
#
Frumpus <- rumpus %>%
  filter(Sex=="f") %>%
  select(Length)
Mumpus <- rumpus %>%
  filter(Sex=="m") %>%
  select(Length)
#On Raw Data, they differ
rumpustest <- t.test(Frumpus,Mumpus, var.equal = TRUE)
rumpustest
#On Logged Data, they differ
rumpustest2 <- t.test(log(Frumpus$Length),log(Mumpus$Length), var.equal = TRUE)
rumpustest2
#Plot on logged data
Loglength <- log(rumpus$Length)
rumpusplot1 <- rumpus %>%
  group_by(Sex) %>%
  summarize(meanLength=mean(Loglength), SE=sd(Loglength)/sqrt(length(Loglength)))
rumpusplot1
#
ggplot(rumpusplot1, aes(x=Sex, y=meanLength, fill=factor(Sex), group=factor(Sex))) +
  geom_bar(stat="identity", position="dodge", size=0.6) +
  geom_errorbar(aes(ymin= meanLength - SE, ymax= meanLength + SE), stat="identity", position="dodge", width = 0.2) +
  theme_minimal() +
  labs(x="Sex", y="Log10 Mean Length of Sparrows", fill="Sex", title="The title") +
  scale_y_log10() +
  scale_fill_manual(values=moma.colors("OKeeffe"), labels=c("m", "f"))
#
ggplot(rumpusplot1, aes(x=Sex, y=meanLength, fill=factor(Sex))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Sex", y="Log10 Mean Length of Sparrows", fill="Sex", title="The title") +
  scale_y_log10() +
  scale_fill_manual(values=moma.colors("OKeeffe"), labels=c("m", "f"))
  
#Trying a different plot
rumpusplot2 <- rumpus %>%
  group_by(Sex) %>%
  summarize(meanlength2=mean(Length), SE2 = sd(Length)/sqrt(length(Length)))
rumpusplot2

ggplot(rumpusplot2, aes(x=Sex, y=meanlength2, fill=factor(Sex), group=factor(Sex))) +
  geom_violin()

ggplot(Bumpus, aes(y=Length, x=Sex, fill=Sex)) + 
  geom_boxplot() + 
scale_fill_manual(values=c("slateblue","darkolivegreen3"), name="Sex") + 
  scale_y_log10() + theme_bw() + 
  labs(y="Length (mm)", x="Sex", title="Length of Male and Female Sparrows") 