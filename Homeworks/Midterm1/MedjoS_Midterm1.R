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
#Trying a different plot on the raw data
ggplot(rumpus, aes(x=Sex, y=Length, fill=Sex)) +
  geom_boxplot() +
  theme_minimal() + scale_y_log10() +
  stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("m", "f"))) +
  labs(x="Sex", y="Length (mm)", fill="Sex", title="Comparison of Body Length Between Female and Male Sparrows") +
  scale_fill_manual(values=moma.colors("Smith"), labels=c("Male", "Female"))
#--c
rm(list=ls())
rumpus <- read_csv("Homeworks/Midterm1/bumpus.csv")
#-Weight is non-normal, we can't use Pearsons
#Spearman's rho
Spearmansrump<-cor.test(rumpus$Weight, rumpus$Length, method="spearman", na.rm=TRUE)
Spearmansrump
#Confirming Spearmans because of tied rank warning <- Spearmans works :)
birdranks <- rumpus %>%
  mutate(weight_rank = min_rank(Weight), length_rank = min_rank(Length))
glimpse(birdranks) 
Pearsonsrump <- cor.test(birdranks$weight_rank, birdranks$length_rank, method="pearson", na.rm=TRUE)
Pearsonsrump
#Kendall's tau
Kendallsrump<-cor.test(rumpus$Weight, rumpus$Length, method="kendall", na.rm=TRUE)
Kendallsrump
#--d
library(lmodel2)
rumpusmodelo2 <- lmodel2(Weight~Length, range.y="relative", range.x="relative", data=rumpus, nperm=99)
rumpusmodelo2
#Scatterlot
ggplot(rumpus, aes(x=Length, y=Weight, color=Length))+
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, color="white") +
  labs(y = "Weight (mg)", x="Length (cm)", title="Sparrow Weight Increases with Body Length") +
  theme_bw(base_size=12) +
  theme(legend.position="right") +
  scale_color_gradientn(colors=moma.colors("Flash"))
#--Sacha Medjo-Akono
#--Midterm 1 - Fall 2024