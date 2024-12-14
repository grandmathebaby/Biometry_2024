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
library(ggsignif)
library(MoMAColors)
library(lme4)
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
              comparisons=list(c("Control","Therapy")),map_signif_level = TRUE,annotations="*")
#--Question 4
rm(list=ls())
Bass <- read_csv("Homeworks/FINAL/habitats.csv")
glimpse(Bass)
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
#
PCAmodel$loadings  # Shows the loadings of each trait on each PC 
#perMANOVA
library("vegan")
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
#
perm <- adonis2(Bass1~Site, Bass, 
                          permutations = 999, method="bray")
perm
perm2 <- adonis(Bass1~Site, Bass, 
                          permutations = 999, method="bray")
summary(perm2)
perm2$coefficients
#
disper <- vegdist(Bass1)
betadisper(disper, Bass$Site)#oknnonumber twice as big
#Multiple Regression
bassmod <- lm(KelpBassDensity~KelpVolume + Temperature + Salinity + Sand +
                Seagrass + Rock, data=Bass)
library(mctest)
imcdiag(bassmod) #No collinearty :)
AIC(bassmod)
AIC(basslogmod) #Best Model
plot(bassmod)
#
anova(bassmod)
#
predicted <- fitted(bassmod)
resid <- residuals(bassmod)
plot(resid~predicted)
abline(h=0) # a bit coney...
#
library(car)
qqp(resid, "norm") #meets normality
#Logged model to see about that cone
basslog <- log((Bass$KelpBassDensity) + 1)
basslogmod <- lm(basslog ~ Temperature + Salinity + KelpVolume + Sand +
                   Seagrass + Rock, data=Bass)
plot(basslogmod)

predicted2 <- fitted(basslogmod)
resid2 <- residuals(basslogmod)

anova(basslogmod)
anova(bassmod)
#--Question 5
rm(list=ls())
dillo <- read_csv("Homeworks/FINAL/armadillos.csv")
glimpse(dillo)
#Logistic Regression
ggplot(dillo, aes(x=Burrows)) + geom_histogram(fill="seagreen") + 
  labs(x="Dillos present?", y="Grain Size") +
  theme_bw()
#
dillo1 <- glm(Burrows ~ GrainSize, family = binomial(link="logit"), data=dillo)
Anova(dillo1) # Yes sig p<0.05
library(pscl)
pR2(dillo1) #McFadden r2 0.1253636 
#Plot
plot(dillo$GrainSize, dillo$Burrows, xlab="Soil Grain Size",ylab="Probability of Armadillo Burrow")
curve(predict(dillo1,data.frame(GrainSize=x),type="resp"),add=TRUE)
#Beautiful Plot
library(ggplot2)
library(dplyr)
to.predict <- data.frame(GrainSize=seq(0.24,1.05,0.01))
predicted.data <- cbind(to.predict, as.data.frame(predict(dillo1, newdata=to.predict, type="response", se=TRUE))) 
glimpse(predicted.data) 
#
ggplot(dillo, aes(x=GrainSize, y=Burrows)) +
  geom_point() +
  geom_line(data = predicted.data, aes(x=GrainSize, y=fit), color="palevioletred") +
  labs(x="Soil Grain Size", y="Presence of Armadillo Burrow",
       title="Armadillos Favor Larger Soil Grain Size for Burrowing") +
  theme_minimal()
#
to.predict <- data.frame(PA_ratio=seq(5,65,5))
predicted.data <- cbind(to.predict, as.data.frame(predict(model1, newdata=to.predict, type="response", se=TRUE)))

glimpse(predicted.data)

ggplot(lizards, aes(x=PA_ratio, y=Present)) +
  geom_point() +
  geom_line(data = predicted.data, aes(x=PA_ratio, y=fit), color="blue") +
  labs(x="Perimeter:Area Ratio", y="Presence of Lizards") +
  theme_bw() 