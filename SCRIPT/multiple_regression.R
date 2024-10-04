# How to do multiple regression in R
# Casey terHorst; edits by Jeremy Yoder
# 2024.09.24

# Use the data set SnailData2. This is similar to what we've used before
# We're going to try to predict what determines snail weight.
# As before, we will use length, but there are two new predictor variables:
# Predator Density and Food Availability

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data, which I have put in a csv file named "SnailData2"
snails <- read.csv("Data/SnailData2.csv")
glimpse(snails)

# First let's see what we would find if we just did 3 simple linear regressions
modL <- lm(Weight ~ Length, data=snails)
modP <- lm(Weight ~ Predators, data=snails)
modF <- lm(Weight ~ AvailableFood, data=snails)

plot(Weight ~ Length, data=snails)
abline(modL)

plot(Weight ~ Predators, data=snails)
abline(modP)

plot(Weight ~ AvailableFood, data=snails)
abline(modF)

# Check that assumptions are met by inspecting residuals
plot(modL)
plot(modP)
plot(modF)
# Looks like all assumptions are met!

# Stats for individual regressions
summary(modL)
summary(modP)
summary(modF)

# Now let's do a multiple regression

# First we want to know which model fits the data best. Let's start with a model
# incorporating all three predictors:
modLPF <- lm(Weight ~ Length + Predators + AvailableFood, data=snails)

# To check for collinearity in your data, we can inspect the data with 
# a pairwise correlations plot, available from the `GGally` package.
# (You may need to install GGally first!)
library(GGally)
X <- snails[,c("Length","Predators","AvailableFood")] # just the columns named
ggpairs(X)

# To get actual numbers on collinearity better test, we can use a function from
# the `mctext` package.
library(mctest)
imcdiag(modLPF)
# This output assesses collinearity through several different statistics. 
# Tolerance (TOL) values as low as 0.1 and VIF values of 10 suggest collinearity

# Now let's make every other possible model, with these three predictors.
# We already have three single-predictor models and the three-predictor model.
# That leaves three possible two-predictor models: 
modLP <- lm(Weight ~ Length + Predators, data=snails)
modLF <- lm(Weight ~ Length + AvailableFood, data=snails)
modPF <- lm(Weight ~ Predators + AvailableFood, data=snails)

# (The order of predictors doesn't matter, just like in addition.
# So `Weight ~ Predators + Length` would be the same as 
# `Weight ~ Length + Predators`.)

# Now let's get AIC for each model.
# Plus, we can use our original simple models above to look at the simplest three models:
AIC(modLPF)
AIC(modLP)
AIC(modLF)
AIC(modPF)
AIC(modL)
AIC(modP)
AIC(modF)

# AIC scores count the number of parameters in the model and subtract the 
# model likelihood score. Smaller AIC values reflect a greater improvement in 
# likelihood for a given number of parameters --- and we want to maximize the
# explanatory power, or "fit", reflected in likelihood while minimizing the 
# factors we use to provide that explanatory power.
# So: lower AIC = better fit
# We can expressly compare our models by subtracting the minimum of their AIC
# scores from each model:

AICs <- c(AIC(modLPF), AIC(modLP), AIC(modLF), AIC(modPF), AIC(modL), AIC(modP), AIC(modF))
AICcomp <- data.frame(model=c("LPF", "LP", "LF", "PF", "L", "P", "F"), AIC=AICs) %>% 
  mutate(dAIC = AIC-min(AICs))
AICcomp
# dAIC, or delta AIC, is the difference between a model's AIC and the lowest AIC 
# seen in any of the competing models. Generally, dAIC > 2 is considered a
# threshold for "better" fit, and dAIC > 4 is "strong evidence" of better fit.
# There are a couple models with dAIC <=2, but the full model has the lowest AIC 
# value, so let's focus on that one.

# Now to get results from the multiple regression. An ANOVA table will give us
# hypothesis tests for the variance explained by each predictor:
anova(modLPF)

# We can quickly check for homogeneity of variances of the full model:
predicted <- fitted(modLPF)
resid <- residuals(modLPF)
plot(resid~predicted)
abline(h=0)
# There isn't a "cone" pattern here, so variance is pretty nicely homogeneous.

# And then check normality of residuals
library(car)
qqp(resid, "norm")
# Also good!

# To get the partial standardized regression coefficients for each factor
library(QuantPsyc)
lm.beta(modLPF)

# Just for fun, here's what the 3D-plot of Weight, Length, and Predators would look like
library(rockchalk)
plotPlane(modLPF, plotx1="Length", plotx2="Predators")

# Also, let's look at what it would look like if we plotted the residuals of 
# Length vs. Predators 
resid <- residuals(modL)
plot(resid~snails$Predators) # after you account for Length, predators don't explain much
plot(resid~snails$AvailableFood) # after you account for Length, Food doesn't explain much

# Now, let's step back to our ANOVA table and our AIC model comparisons:
anova(modLPF)
# As we saw, Length is the strongest predictor of weight, and the other two
# predictors make very small predictive contributions.
AICcomp
# And, indeed, a model explaining weight with length has dAIC < 2, so it's not
# significantly worse-fit than the full model! You can also see that models 
# including food availability are often not significantly worse-fit than the 
# full model, and food did have a marginally significant effect in the full 
# model. Finally, models with predator density but not one or both of the other 
# variables have
# dAIC >> 4 --- the low predictive power is showing, there.

