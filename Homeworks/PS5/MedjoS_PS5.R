#--Sacha Medjo-Akono
#--Problem Set 5 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
library(lme4)
library(pscl)
library(factoextra)
library(MoMAColors)
display.all.moma()
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
ggplot(Herb, aes(y=Fruits, x=Range, fill=Range)) + 
  geom_boxplot() + 
  scale_fill_manual(values=moma.colors("Warhol"), labels=c("Invasive", "Native"),
                             name="Range") + 
  facet_wrap("Disturbance") + scale_y_log10() + theme_minimal() + 
  labs(y="Fruit production", x="Range", title="Medicago Genotype Affects Fruit Production in Response to \nDisturbance Within Collection Ranges") +
  theme(legend.position="bottom")
#--Question 3
rm(list=ls())
surf <- read_csv("Homeworks/PS5/surfperch_mating.csv")
View(surf)
#
qqp(surf$`male size`, "norm") #thank god
#Binomial model because y/n is 0/1
surfmod1 <- glm(mated~`male size`, family=binomial(link="logit"), data=surf)
Anova(surfmod1, type="III")
pR2(surfmod1)
#Plot
to.predict <- data.frame(`male size`=seq(9,22,1), stringsAsFactors = FALSE)
# Ensure 'male size' is treated correctly
to.predict <- setNames(to.predict, c("male size"))
predicted.data <- cbind(to.predict, as.data.frame(predict(surfmod1, newdata=to.predict, type="response", se=TRUE)))
glimpse(predicted.data)
#
ggplot(surf, aes(x=`male size`, y=mated, color=factor(mated))) +
  geom_point() +
  geom_line(data = predicted.data, aes(x=`male size`, y=fit), color="black") +
  scale_color_manual(values=moma.colors("Andri"), labels=c("0", "1"),
                     name="mated") + 
  labs(x="Male Size", y="Mating Success", title="Larger Male Surfperches More Successful in Mating") +
  theme_minimal()
display.all.moma()
#--Question 4
#There are 621 fish
rm(list=ls())
female <- 371
male <- 250

observed <- matrix(c(female, male), nrow=1, ncol=2)

expected <- matrix(c((female+male)/2, (female+male)/2), nrow=1, ncol=2)

X2test <- sum((observed-expected)^2/expected)
X2test
1-pchisq(X2test, df=1)
#--Question 5
rm(list=ls())
wild <- 161
mutant <- 33

observed <- matrix(c(wild, mutant), nrow=1, ncol=2)
expected <- matrix(c((wild+mutant)*0.75, (wild+mutant)*0.25), nrow=1, ncol=2)
#G test
Gvalue <- 2*sum(observed*log(observed/expected))
Gvalue 
1-pchisq(Gvalue, df=1)
#Chisquared
X2test <- sum((observed-expected)^2/expected)
X2test
1-pchisq(X2test, df=1)
#--Question 6
rm(list=ls())
#Chisquared
hanging <- matrix(c(58, 33, 61, 21), nrow=2, byrow=TRUE)
hanging
colnames(hanging) <- c("Water", "Gatorade")
rownames(hanging) <- c("No Headache", "Headache")

chi_square_test <- chisq.test(hanging)

chi_square_test

#GLM method
stpattys <- data.frame(
  Drink = factor(c(rep("Water", 119), rep("Gatorade", 54))),
  Headache = factor(c(rep("No", 58), rep("Yes", 61), rep("No", 33), rep("Yes", 21)))
)
#GLM Model
hungover <- glm(Headache ~ Drink, family=binomial, data=stpattys)
# Summary of the model
summary(hungover)
#
anova(hungover, test="Chisq")
#--Question 7
rm(list=ls())
#GLM model
Poll <- data.frame(
  race = c("White", "White", "Black", "Black", "Latino", "Latina", 
           "White", "White", "Black", "Black", "Latino", "Latina"),
  sex = c("Men", "Women", "Men", "Women", "Men", "Women", 
          "Men", "Women", "Men", "Women", "Men", "Women"),
  candidate = c("Biden", "Biden", "Biden", "Biden", "Biden", "Biden", 
                "Trump", "Trump", "Trump", "Trump", "Trump", "Trump"),
  votes = c(2588, 3907, 1007, 1616, 774, 1017, 5177, 5239, 160, 69, 393, 510)
)
#
votemod <- glm(votes~candidate:sex + candidate:race:sex,
               family=poisson, data=Poll)

# And then use a modified ANOVA, testing against a chi-squared distribution:
anova(votemod, test="Chisq")
#--Sacha Medjo-Akono
#--Problem Set 5 - Fall 2024