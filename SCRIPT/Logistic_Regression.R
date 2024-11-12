# How to do logistic regression
# Casey terHorst, revisions by Jeremy Yoder
# 2024.11.04

# This example examine the effect of perimeter to area ratio (PA) of islands on 
# the presence of lizards on the island

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data
lizards <- read.csv("Data/lizards.csv")
glimpse(lizards)

ggplot(lizards, aes(x=Present)) + geom_histogram(fill="seagreen") + 
  labs(x="Lizards present?", y="Islands") +
  theme_bw()
# Notice that the data is binomially distributed. There are only two possible 
# outcomes, present (1) or absent (0). So we can fit a binomial error 
# distribution to our data.

library(lme4)
library(car)
model1 <- glm(Present ~ PA_ratio, family = binomial(link="logit"), data=lizards)
Anova(model1)

# We'd also like to get a pseudo R^2 value that tells us how much of the variance
# in lizard presence is explained by the PA ratio of the island

library(pscl)
pR2(model1) # gives pseudo R^2
# The last three columns are all different types of pseudo-R^2s.
# I usually use McFadden's rho-square

# To plot the data
# A simple plot
plot(lizards$PA_ratio,lizards$Present,xlab="Perimeter:Area Ratio",ylab="Probability of Lizard Presence")
curve(predict(model1,data.frame(PA_ratio=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model

# We can infer from the graph that islands with lower perimeter to area ratios 
# have a greater chance of lizards being present.

# Here's some code for making a nicer plot with ggplot
to.predict <- data.frame(PA_ratio=seq(5,65,5))
predicted.data <- cbind(to.predict, as.data.frame(predict(model1, newdata=to.predict, type="response", se=TRUE)))

glimpse(predicted.data)

ggplot(lizards, aes(x=PA_ratio, y=Present)) +
  geom_point() +
  geom_line(data = predicted.data, aes(x=PA_ratio, y=fit), color="blue") +
  labs(x="Perimeter:Area Ratio", y="Presence of Lizards") +
  theme_bw() 

### TRY IT YOURSELF
# Joshua trees, like many long-lived plant species, follow a "masting" strategy
# for reproduction, in which the trees flower intensively in some years and go
# without flowering in other years. This is thought to be an adaptation to
# overwhelm seed predators and maximize the survival of seeds and seedlings.
# Populations of masting species synchronize their reproductive effort by
# responding to cues in weather, and for a desert plant like Joshua tree, we'd
# expect that the cues include changes in precipitation and winter temperatures
# --- the trees are thought to flower when a wet year follows a dry year, and
# when there's been a strong winter frost.
  
# You collect records of whether or not western Joshua trees (Yucca brevifolia) 
# have flowered in a variety of locations across their range, over ten years from
# 2013 to 2022. Locations are in 4km^2 raster grid cells, and assumed to be
# independent observations of local Joshua trees' flowering activity. For each
# location grid cell, you obtain the difference in precipitation between the
# year of observation (Y0) and the previous year (Y1), and the minimum annual
# temperature for the year of observation (Y0). These data are in the file 
# `Yucca_brevifolia_flowering.csv`, with flowering status coded as flowering (1)
# or not flowering (0).
  
# Use logistic regression to test for a significant relationship between 
# annual precipitation contrasts, and minimum temperature --- and whether 
# variation in sampling effort from year to year mean that a random effect of 
# observation year is necessary. When you have a final model selected, plot
# the relationship between flowering and the fixed-effect predictors in your
# model.
  
# YOUR CODE HERE
  

