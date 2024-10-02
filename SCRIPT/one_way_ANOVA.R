# Code for doing simple ANOVA in R
# Casey terHorst, revisions by Jeremy Yoder
# 2024.10.02

# Again we will start with our old friend, the Snail Data

# Let's say we want to know the effect of tidal height (High or Low) on snail length
# We're going to ignore the other variables for now.

# Clear the environment, load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data. 
snails <- read.csv("Data/SnailData.csv")
glimpse(snails)

# Double check to make sure that TidalHeight is a character or factor
class(snails$TidalHeight)

# It is
# !! If your factor is numeric, R will try to run a regression instead of ANOVA

# We fit the ANOVA model in the same way we fit a regression model
# The syntax is always "dependent_variable ~ factor"

model1 <- lm(Length ~ TidalHeight, data = snails)

#Now let's check our assumptions
plot(model1)

#I'm not 100% sure about the normal probability plot. Let's try it with confidence intervals
library(car)
resid1 <- residuals(model1)
qqp(resid1, "norm")

# Ok, that's not as bad as I thought. Let's go with it.

# To get the results of our model:
summary(model1)

# At the bottom, you get F, df, and P, which is what you need.
# For a one-way ANOVA, this is all you need. As we get into more complex ANOVAs, 
# this won't be that helpful, as it tells you how well the whole model fits, but
# not how each individual factor explains the response.

# For future use, if you want to get the ANOVA table, then use:
anova(model1)

# Note that this gives you the same information, but it's in the form of an 
# ANOVA table

# Now, because we only had two levels of Tidal Height, we know the difference 
# has to be between High and Low. As an example for how we can use a Tukey test
# to examine pairwise differences between groups when there are more than two
# groups, let's call on the Palmer Penguins again:
library("tidyverse")
library("palmerpenguins")
glimpse(penguins)

# First, take a look at variation in bill depth among the three penguin species 
# in the data set:
ggplot(penguins, aes(x=species, y=bill_depth_mm)) + geom_boxplot()

# Gentoos seem to have much shallower bills than Adelie and Chinstrap penguins!
# To test this, fit a model to compare bill depth among species.
model2 <- lm(bill_depth_mm~species, data=penguins)
anova(model2)

# There's a significant effect of species on bill depth, but is it mainly driven
# by the Gentoos' difference from the other two? We can see this with a Tukey
# test of the contrasts between pairs of groups.

# To do a post-hoc Tukey test, we can use the emmeans package
library("emmeans")
emmeans(model2, pairwise~"species", adjust="Tukey")
# This gives you mean, SE, and CIs within species, then tests of contrasts 
# between each pair of species.

TukeyHSD(aov(model2)) # these base R functions will give you just the contrasts

# Could also use this, which conveniently gives letters for different groups based
# on the contrasts ...
library("agricolae")
HSD.test(model2, "species", console=TRUE) 
# You can take these letters and apply them to your graph

# To plot the data, you can can easily gather means and SE with emmeans
graphdata <- as.data.frame(emmeans(model2, "species"))
graphdata

ggplot(data=graphdata, aes(x=species, y=emmean, fill=species)) +
  geom_bar(width=0.5, stat="identity") + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), color="black", stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086')) +
  guides(fill="none") + 
  ylab("Bill depth (mm)") +
  xlab("Species") +
  theme_bw(base_size=14)
  
# You could just export that graph and put it into Powerpoint or Illustrator
# to add the letters to the bars. Here I'm going to do it in R with geom_text

# I'm going to append a new column to graph data with the Tukey results in order
graphdata$tukey <- HSD.test(model2, "species", console=FALSE)$groups$groups  
# This pulls the group labels from the `agricola` package's HSD function into our
# data frame for the figure ...
graphdata

ggplot(data=graphdata, aes(x=species, y=emmean, fill=species)) +
  geom_bar(width=0.5, stat="identity") + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), color="black", stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  geom_text(aes(label=tukey), vjust=-2) + # You may need to tweak `vjust` and y limits to make this work
  scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086')) +
  ylim(0,20) +
  guides(fill="none") + 
  ylab("Bill depth (mm)") +
  xlab("Species") +
  theme_bw(base_size=14)

# Okay --- there's our answer. Gentoos have significantly shallower bills than 
# the other two species, but those other two aren't significantly different in
# bill depth. That one contrast is driving the significant species effect in the
# one-way ANOVA.

# Try this out yourself with a new measurement from the penguin data. Inspect 
# species differences for different measures, then pick one and test the hypothesis
# that the species significantly differ in that measure, then check the pairwise
# contrasts and plot the result.

# YOUR CODE HERE


