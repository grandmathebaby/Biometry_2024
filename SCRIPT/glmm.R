# Generalized Linear Mixed Models in R
# Casey terHorst, revisions by Jeremy Yoder
# 2024.10.29

# We're going to use the Plant Competition data from Problem Set 4, Question 2
# This looked at the effects of Clipping, Weeding, and Species Identity on the 
# number of flowers that plants produced.

# The dependent variable is the number of flowers, which is not normally-distributed.
# In Problem Set 4, we resolved this by log-transforming the data.
# Here we will fit a generalized linear model with an appropriate error distribution

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data
plants <- read.csv("Data/plantcompetition.csv")
glimpse(plants)

# Before we proceed, it's important to know that many of the non-normal 
# distributions do not handle zeroes or negative numbers well (e.g. gamma). They
# only deal with positive numbers. So because there are 0's in this data set, 
# I'm going to add 1 to everything. This is valid because it's done to ALL 
# values in the dataset.
plants$flowers1 <- plants$flowers+1

# STEP 1: Determine the best error distribution
library(car)
# First, try a normal distribution
qqp(plants$flowers, "norm") 
# Ok, definitely not normal!

# So let's try a lognormal distribution
qqp(plants$flowers1, "lnorm")
# That looks okay. Let's also try gamma ...

# Checking the qqplots for other distributions gets a little more involved:
library("MASS")
gamma <- fitdistr(plants$flowers1, "gamma")
qqp(plants$flowers1, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
# not quite as good as log-normal

# Try negative binomial (appropriate only for discrete variables, like counts)
nbinom <- fitdistr(plants$flowers1, "Negative Binomial")
qqp(plants$flowers1, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# not as good as log either

# Try poisson (appropriate only for discrete variables, like counts)
poisson <- fitdistr(plants$flowers1, "Poisson")
qqp(plants$flowers1, "pois", lambda=poisson$estimate)
# nope

# Looks like lognormal is our best choice


# STEP 2: fit a model with our chosen error distribution
# To fit a normal distribution in a glm, it would look like this

library(lme4) # We need to use glm instead of lm
model1 <- glm(flowers1~clipped*weeded*Species, family=gaussian(link="identity"), data=plants)


#But we really wanted to fit a log-normal distribution. 
#To do that, we keep gaussian, but change the link to log
model2 <- glm(flowers1~clipped*weeded*Species, family=gaussian(link="log"), data=plants)


# If we wanted to fit Gamma instead:
model3 <- glm(flowers1~clipped*weeded*Species, family=Gamma(link="inverse"), data=plants)
#can also try using a log link function for Gamma


# Now to get results from the lognormal model
Anova(model2, type="III") # This does Likelihood Ratio Tests on each factor


# Ok, now let's say we wanted to make Species a random effect instead of a 
# fixed one. So now we're fitting a mixed model, with two fixed factors 
# (clipped and weeded) and one random factor (Species). I'm not arguing that 
# Species SHOULD be a random effect, but just want you to have the example.

# To fit a mixed model, we have to use glmer instead of glm, which lets us 
# specify random effects in a generalized linear model:

model4 <- glmer(flowers1~clipped + weeded + clipped:weeded + (1|Species) + (1|clipped:Species) + (1|weeded:Species) 
             + (1|clipped:weeded:Species), family=gaussian(link="log"), data=plants)
# We may get a "boundary (singular)" warning here, but carry on

Anova(model4, type="III")

# To test random effects, create new models with and without the random effect
# For example, to test the significant of the weeded:Species interaction, we'd 
# create new models that differ ONLY in that effect
model5 <- glmer(flowers1~clipped + weeded + clipped:weeded + (1|Species) + (1|clipped:Species) + (1|weeded:Species), family=gaussian(link="log"), data=plants)
model6 <- glmer(flowers1~clipped + weeded + clipped:weeded + (1|Species) + (1|clipped:Species), family=gaussian(link="log"), data=plants)

anova(model5, model6) # result is significance of the random effect

# Looks like model5 is not a significantly better fit, which means that 
# weeded:Species does not significantly improve model fit

# If we wanted to just test the random Species effect, then:
model7 <- glmer(flowers1~clipped + weeded + clipped:weeded + (1|Species), family=gaussian(link="log"), data=plants)
model8 <- glm(flowers1~clipped + weeded + clipped:weeded, family=gaussian(link="log"), data=plants)
anova(model7, model8)
# Now model 7 is a better fit, which tells us that Species has a significant 
# effect on flower number


# TRY IT YOURSELF

# Take a stab at this modeling framework with one of our least normal example
# datasets, the counts of floral visitor species in plant community surveys.

visits <- read.csv("Data/degree_per_plant.csv")
glimpse(visits)

# Remember, in this data, each observation is the number of animal species
# visiting flowers of a particular plant species with zygomorphic or actinomorphic
# floral symmetry, in different natural communities (identified as by "matrix" ID 
# in the data table). Count data is better modeled, not as log-transformed, but
# as Poisson distributed, and possibly with a log link. 

# You'll want to fit a model with a random effect of "matrix" to account for 
# differences among plant communities, so use the generalized mixed model 
# function `glmer()`. Fit models to test the effect of floral symmetry on visitor
# species count, and evaluate different possible error distributions and link
# functions, as well as predictor sets.


