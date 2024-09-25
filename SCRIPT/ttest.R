# Code for t-tests and checking assumptions
# Casey terHorst, updates by Jeremy Yoder
# 2024.09.15

#Clear the environment
rm(list=ls())

# load useful libraries
library("tidyverse")

# Import the data. Going to use SnailData again as an example
mydata <- read.csv("Data/SnailData.csv")
View(mydata)

# Visualize the data to refresh your memory of the major factors
ggplot(mydata, aes(x=Weight)) + 
  geom_histogram(bins=6, fill="darkorchid") +
  theme_minimal()

ggplot(mydata, aes(y=Weight, x=TidalHeight, fill=TidalHeight)) + 
  geom_boxplot(alpha=0.5) +
  geom_jitter(width=0.2) + 
  facet_wrap("Location") +
  theme_minimal()

# Run a one sample t-test. Let's say that 2 years ago, the mean weight of 
# snails at these sites was 10g. Now we want to know if the mean snail weight 
# is different from 10.

# This is a job for a one-sample t-test, via the t.test() function. The `mu`
# option in the function sets the value for comparison to the sample, and 
# because we have some missing data, we also set na.rm=TRUE.
mytest <- t.test(mydata$Weight, mu=10, na.rm=TRUE)
mytest

# Yes, the weight is different from zero. The p-value reflects the probability 
# that the population mean is equal to 10. (This is the null hypothesis.) A low 
# p-value therefore supports the hypothesis that the mean is different from 10.

# Let's say we want see if the snails' weights are different at the two sites.
# This is now a question for a two-sample t-test. We'll start with the 
# assumption that the sample variances are equal across sampling sites (setting 
# `var.equal=TRUE`), though we haven't formally tested this.
mytest2 <- t.test(Weight~Location, var.equal=TRUE, data=mydata, na.rm=TRUE)
mytest2

# No, they're not different. Here, the p-value reflects the probability that
# the samples come from populations with the same mean. A low p-value would
# support the hypothesis that the samples are from populations with different
# mean values.

# As noted above, we never confirmed that the samples had sufficiently similar
# variances to assume they were equal. If we don't want to make that assumption
# we can use Welch's t-test: a two-sample t-test test that allows for separate 
# variances. This is a slightly more conservative test, and usually the one you 
# should use by default.
mytest3 <- t.test(Weight~Location, data=mydata, na.rm=TRUE)
mytest3

# A paired t-test isn't appropriate for the snail data, because it's not 
# organized in that way. Paired samples should be contrasts within a larger 
# sampling unit: the same individual measured before and after a treatment, 
# for instance. We'll demonstrate a paired t-test with data from Yoder et al 
# 2020, comparing the number of floral visitors for plants with actinomorphic 
# (radial) or zygomorphic (bilateral) floral symmetry.

flowers <- read.csv("Data/floral_visitors.csv")
# This records the mean number of visitor species observed for plant species in larger
# communities (identified by "matrix") with zygomorphic and actinomorphic flowers 
View(flowers) 

# The variable mnPoll is the mean number of pollinator (floral visitor) species 
# for flowers with a particular symmetry in a particular community (matrix)
# This variable is more or less log normal --- 
# compare a histogram of the un-transformed data
ggplot(flowers, aes(x=mnPoll)) + geom_histogram()
# to a histogram with log scaling
ggplot(flowers, aes(x=mnPoll)) + geom_histogram() + scale_x_log10()

# These data are also naturally paired. We want to know whether flowers with 
# different symmetry have different numbers of visitor species --- and we have
# flowers of both symmetry types in most of the communities in the data set.
# Here's a ggplot figure that shows contrasts across pairs in a nice way:

ggplot(flowers, aes(x=symmetry, y=mnPoll, group=matrix, color=symmetry)) + 
  geom_line(color="gray50", alpha=0.5, linewidth=1) +
  geom_point(size=6, alpha=0.3) +
  scale_color_manual(values=c("#af8dc3", "#7fbf7b")) + 
  scale_y_log10() +
  labs(x="Floral symmetry", y="Mean visitor species") +
  theme_minimal(base_size=18) +
  theme(legend.position="none")

# That kind of contrast is appropriate for a paired t-test.

# To conform to the t-test assumption of normally distributed data, we want to
# test not on the mean number of floral visitor species but on the log-
# transformed number. We can get those transformed values by making a new 
# column in the data frame:

logflowers <- flowers %>% mutate(logMnPoll=log10(mnPoll))
glimpse(logflowers)

# We can also transform data within the t.test function call, by wrapping mnPoll 
# in a `log10()` function. Whether you do this or create a new column with the
# transformed values would depend on your larger data analysis plan --- do you
# expect to use the transformed values for multiple analyses? Then you probably
# want to create a new column. Otherwise, put the transformation in the test
# function call.

mytest4 <- t.test(log10(flowers$mnPoll[flowers$symmetry=="actinomorphic"]),log10(flowers$mnPoll[flowers$symmetry=="zygomorphic"]), paired=TRUE, na.rm=TRUE)
mytest4

# Ok, let's go back to mytest2. We assumed that the data were normally 
# distributed AND we assumed that the variances were equal. So let's test those 
# assumptions. We could explore the data graphically, as we have before.
library(car)
qqp(mydata$Weight, "norm")
# looks pretty normal!

# If we wanted to do a formal test of normality, we'd use a Shapiro test
shapiro.test(mydata$Weight)
# If P>0.05, then the data are normal
# I don't recommend this test typically though, as it's pretty conservative, 
# and can throw false positives. (Or, at least, it can diagnose non-normal data
# that are not non-normal enough to throw off a test.)

# How do we test whether the variances between groups are equal?
# One way is to just look at the variances:
Variance_Snails<- mydata %>%
  group_by(Location) %>%
  summarize(variance=var(Weight, na.rm=TRUE))
Variance_Snails
# Those are pretty close. But how do we know how different is TOO different?

# We can do a boxplot, which also allows us to check for outliers
boxplot(Weight~Location, data=mydata)
# Still looks pretty good. 

# Finally, we can do a formal test, if we want to, using a Bartlett Test
bartlett.test(Weight~Location, data=mydata)
# If P>0.05, then variances are equal
# I don't normally recommend this, however; again, it can be over-sensitive.
# Generally, visual inspection or comparison of estimated variances is enough.

# TRY IT YOURSELF
# Okay, now that you've had a run through t-tests, try them out with a new set
# of observations. For this we'll load the "Palmer Penguins" data resource, which
# has morphological measurements for three species of penguins near the Palmer
# Research Station in Antarctica https://allisonhorst.github.io/palmerpenguins/

# if you haven't run this before, you'll need to run
# install.packages("palmerpenguins")
library("palmerpenguins") 

glimpse(penguins) # take a look at the data

table(penguins$species)
# There's data for three penguin species, but t-tests are only appropriate for
# differences between at most two groups. Pick two of the three penguin species
# and use filter() to create a new data.frame containing just those species ...

#Filter eg run ?filter

# YOUR CODE HERE
Twopenguins <- penguins %>% filter(species !="Adelie") #removes the item mentioned

Twopenguins <- penguins %>% filter(species %in% c("Adelie", "Gentoo")) #includes only items mentioned

table(Twopenguins$species)

# Then pick one of the morphological measures and test whether the species 
# samples for that measure meet the assumptions of a default two-sample t-test. 
# Use the results of that test to choose your method for determining whether 
# the two species differ in that measure more than we expect due to chance.

#my code
penguintest1 <- t.test(body_mass_g~species, data=Twopenguins, na.rm=TRUE)
penguintest1

penguintest2 <- t.test(body_mass_g~species, var.equal=TRUE, data=Twopenguins, na.rm=TRUE)
penguintest2
