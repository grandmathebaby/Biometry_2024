# How to do ANCOVA (Analysis of Covariance)
# Casey terHorst, revisions by Jeremy Yoder
# 2024.10.20

# We'll turn once again to our Snail Data example again

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data
snails <- read.csv("Data/SnailData.csv")
glimpse(snails)

# Before we start, let's get rid of the annoying NA
snails <- snails[-26,]

# We'll have two predictor variables for snail weight: TidalHeight, a discrete 
# variable, which needs to be a factor, and length, a continuous variable, which 
# imported as a numeric vector (good)
snails$Location <- as.factor(snails$Location)
snails$TidalHeight <- as.factor(snails$TidalHeight)

# Now we can just run our linear model using these two predictor variables.
model1 <- lm(Weight ~ TidalHeight * Length, data=snails)

# Check assumptions
plot(model1)
# These look good, but if not, we'd need to transform

# Remember that you also want to make sure that the relationship with covariate 
# (the continuous predictor) is linear
plot(Weight~Length, data=snails)
# Looks good!
# If the relationship does not look linear, then we'd need to transform one or 
# both variables

# If the design is balanced, then we can just ask for results from our model
anova(model1)

# If you have an unbalanced design, then you should use Type III SS instead
library(car)
Anova(model1, type="III")

# You can stop right there if you want, but if you wanted to go further and do 
# model selection, here's what we'd do. The interaction term is highly 
# non-significant, so you might want to formally evaluate whether or not to 
# drop it. Let's look at AIC for models with and without the interaction:

model2 <- lm(Weight ~ TidalHeight + Length, data=snails)

AIC(model1)
AIC(model2)

# The model without the interaction is a tiny bit better, but about 2 AIC units,
# which is only marginally better. You would be justified in going with the 
# simpler model, but only in the name of Occam's Razor.

# Here's how we plot the data to illustrate the interaction

predWeight <- predict(model1) 
# Gets the predicted values from the regression lines in the ANCOVA

graphdata <- cbind(snails, predWeight) 
# attaches those predictions to the dataset

# We'll plot the original data as points, and the regression predictions
# as lines --- and in this case you can see that the relationship between
# length and weight is very very similar at the two tidal heights
ggplot(data=graphdata, aes(x=Length, y=Weight, color=TidalHeight)) +
  geom_point() + geom_line(aes(y=predWeight)) +
  labs(x="Length", y="Weight", fill="TidalHeight") +
  theme_bw() 


