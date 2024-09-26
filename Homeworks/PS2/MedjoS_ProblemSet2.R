#--Sacha Medjo-Akono
#--Problem Set 2 - Fall 2024
rm(list=ls())
urchins <- read_csv("Homeworks/PS2/Urchins.csv")
View(urchins)
##load packages
library(tidyverse)
library(car)
library(psych)
library(moments)
library(MoMAColors)
##--1
UFlat <- urchins  %>%
  filter(Location == "Flat")
UComplex <- urchins %>%
  filter(Location == "Complex")
##
flatmean <- mean(UFlat$Density)
complexmean <- mean(UComplex$Density)
##--
Varflat<-var(UFlat$Density)
Varcomplex<-var(UComplex$Density)
##--
SDflat<-sqrt(Varflat)
SDcomplex<-sqrt(Varcomplex)
##--1b
UPooled <- t.test(Density~Location, var.equal=TRUE, data=urchins)
UPooled
##
USep <- t.test(Density~Location, data=urchins)
USep
##
ggplot(urchins, aes(y=Density, x=Location, fill=Location)) + 
  geom_boxplot(alpha=0.5) +
  geom_jitter(width=0.2) + 
  labs(
    title = "Increased Urchin Density on Rocks with Complex Topography",
    x = "Rock Topography",
    y = "Urchin Density"
  ) +
  theme_minimal() +
  scale_fill_manual(values = moma.colors("Fritsch", 2)) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )

##--2
rm(list=ls())
Wheat <- read_csv("Homeworks/PS2/CoastBuckwheat.csv")
View(Wheat)
#--Data is normal
shapiro.test(Wheat$Density)
##--One tailed t-test because we only have one eg of constant
Wheatest <- t.test(Wheat$Density, mu=4, na.rm=TRUE)
Wheatest
qqnorm(Wheat$Density)
qqline(Wheat$Density)
#--Graph
SortedWheat <- sort(Wheat$Density)
SortedWheat
##-- I tried without transforming data
lowCI <- 4.366935 #From Shapiro test
highCI <- 6.766398
lowCI
highCI
##-- If I wanted to do it "by hand"
#lowCI is mean(Wheat$Density)-(qt(0.975,29)*(sd(Wheat$Density)/sqrt(30)))
#highCI is mean(Wheat$Density)+(qt(0.975,29)*(sd(Wheat$Density)/sqrt(30)))
##--Plotting
ggplot(data.frame(x=Wheat$Density), aes(x=x)) +
  geom_histogram(fill="lightblue", color="black", bins=10) +
  labs(x="Samples", y="Wheat Density", title="Coastal Buckwheat Density Mean Exceeds Standard After Restoration") +
  theme(legend.position="right") +
  geom_vline(aes(xintercept = lowCI, color="Lower CI 4.37")) +
  geom_vline(aes(xintercept = highCI, color="Upper CI 5.77")) +
  geom_hline(aes(yintercept = 4, color="Standard Mean (4 plants/25m^2)")) +
  guides(color = guide_legend(title = "Legend"))
##-- Bootstrapping
WheatBoots <- replicate(1000, {
  samples <- sample(Wheat$Density,replace=TRUE);
  mean(samples)  })
WheatBoots
SortedWB <- sort(WheatBoots)
LowBoot <- SortedWB[25]
HighBoot <- SortedWB[975]
LowBoot
HighBoot
##--Plot
ggplot(data.frame(x=SortedWB), aes(x=x)) +
  geom_histogram(fill="lightblue", color="black") +
  labs(x="Bootstrapped samples", y="Wheat Density", title="Coastal Buckwheat Density Mean Exceeds Standard After Restoration") +
  theme(legend.position="bottom") +
  geom_vline(aes(xintercept = LowBoot, color="Lower CI")) +
  geom_vline(aes(xintercept = HighBoot, color="Upper CI")) +
  geom_hline(aes(yintercept = 4, color="Standard Mean (4 plants/25m^2)")) +
  guides(color = guide_legend(title = "Legend"))

##--3
rm(list=ls())
RT <- read_csv("Homeworks/PS2/RunTimes.csv")
RT
#
shapiro.test(RT$Water)
var.test(RT$Sportsdrink,RT$Water)
#
RTtest <- t.test(RT$Water, RT$Sportsdrink, paired=TRUE, data=RT)
RTtest

##--4
rm(list=ls())
crybbs <- read_csv("Homeworks/PS2/crying_babies.csv")
view(crybbs)
##
ggplot(crybbs,aes(crybbs$cryduration,crybbs$IQ)) +
  geom_point(size=2) +
  labs(
    title = "IQ's of Crying Babies in Relation to Their Crying Times",
    x = "Crying time (mins)",
    y = "IQ",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.6),
  )
##--Normally distributed
shapiro.test(crybbs$cryduration)
shapiro.test(crybbs$IQ)
##
Ranks <- crybbs %>% mutate(cryrank = min_rank(cryduration), iqrank = min_rank(IQ))
glimpse(Ranks)
##--Pearsons r
Pearsonsbbys <- cor.test(crybbs$cryduration,crybbs$IQ, method = "pearson")
Pearsonsbbys
#--Spearman's rho
Spearmansbbys <- cor.test(Ranks$cryrank, Ranks$iqrank, method = "spearman")
Spearmansbbys
##--Kendall's tau
Kendallsbbys <- cor.test(Ranks$cryrank, Ranks$iqrank, method = "kendall")
Kendallsbbys

##--5
rm(list=ls())
Floridaman <- read_csv("Homeworks/PS2/butterflyballot.csv")
view(Floridaman)
##-
NoPalm<- Floridaman[-67,]
glimpse(NoPalm)
shapiro.test(NoPalm$Bush)
shapiro.test(NoPalm$Buchanan)
##-Without Palm
##--Pearsons r
Pearsons <- cor.test(NoPalm$Bush,NoPalm$Buchanan, method = "pearson")
Pearsons
#--Spearman's rho
Spearmans <- cor.test(NoPalm$Bush,NoPalm$Buchanan, method = "spearman")
Spearmans
##--Kendall's tau
Kendalls <- cor.test(NoPalm$Bush,NoPalm$Buchanan, method = "kendall")
Kendalls
##-- With Palm
##--Pearsons r
Pearsons2 <- cor.test(Floridaman$Bush,Floridaman$Buchanan, method = "pearson")
Pearsons2
#--Spearman's rho
Spearmans2 <- cor.test(Floridaman$Bush,Floridaman$Buchanan, method = "spearman")
Spearmans2
##--Kendall's tau
Kendalls2 <- cor.test(Floridaman$Bush,Floridaman$Buchanan, method = "kendall")
Kendalls2

##--
rm(list=ls())
Streams <- read_csv("Homeworks/PS2/Streams.csv")
view(Streams)
##
##--Plot
ggplot(Streams, aes(x=NumberSpp, y=Biomass)) +
  geom_point(size=2) +
  geom_smooth(method="lm", se=)
  scale_fill_manual(values=c(moma.colors("Connors", 3)), name="Biomass") + 
  labs(x="Streams", y="Biomass", title="the biomass is changing") +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Legend"))
