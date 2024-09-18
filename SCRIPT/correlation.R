# Code for different ways to do correlations
# Casey terHorst, revisions by Jeremy Yoder
# 2024.09.18

# Clear the environment
rm(list=ls())

# load the tidyverse
library("tidyverse")

# Import the data. Going to use SnailData again as an example
mydata <- read.csv("Data/SnailData.csv")
glimpse(mydata)

# Let's look at correlations between length and weight
# First let's just look at the relationship between the two variables
plot(Weight~Length, data=mydata)

# Longer snails are heavier! Is this relationship stronger than we'd expect 
# due to chance, though?

# Check for normality of each variable --- this will determine what tests are
# appropriate
library(car)
qqp(mydata$Length, "norm")

qqp(mydata$Weight, "norm")

# Weight is normal. Length is pretty close (good enough for us)


# Pearson's correlation test
mytest1 <- cor.test(mydata$Weight, mydata$Length, method="pearson", na.rm=TRUE)
mytest1
# This correlation method compares the data to a t-distribution to determine
# whether it's greater than expected due to chance. The last number in the 
# output is the correlation coefficient, r.

# Spearman's rho
mytest2 <- cor.test(mydata$Weight, mydata$Length, method="spearman", na.rm=TRUE)
mytest2
# Notice that you'll get a warning because of ties. Spearman's correlation tests
# for a relationship between RANKS, not between actual data values. This is useful
# if your data are non-normal or you're worried about the impact of outliers.
# Tied values means if you get a marginal p-value, you should worry a little, but 
# otherwise it's ok. Note that the test runs anyway. 

# (This is usually true with "warnings" in R, but not errors.)

# We can confirm that Spearman's correlation test works the way we expect
# by explicitly creating ranks in the data, and estimating the correlation 
# between those ranks.
snailranks <- mydata %>% mutate(weight_rank = min_rank(Weight), length_rank = min_rank(Length))
glimpse(snailranks) # that created two new columns with rankings
mytest2.5 <- cor.test(snailranks$weight_rank, snailranks$length_rank, method="pearson", na.rm=TRUE)
mytest2.5 

# This should look very similar to the results in mytest2.

# Kendall's tau is another option for non-normal data, and we can also access
# this method in the `cor.test()` function:
mytest3 <- cor.test(mydata$Weight, mydata$Length, method="kendall", na.rm=TRUE)
mytest3
# Same warning message as above, which means more or less the same --- it's not 
# a problem for these data.

# Finally, correlation tests are another case like t-tests, in which you may
# have an expectation about the direction of the effect before you run the test.
# That is, a positive (or negative) correlation may support your alternative 
# hypothesis while a negative (or positive) correlation would not. If this is
# true, you can use a one-tailed version of the correlation test to increase
# its power to detect a correlation that supports your alternative hypothesis.
# We do this by setting the `alternative` option

mytest4 <- cor.test(mydata$Weight, mydata$Length, method="pearson", alternative="greater")
# `alternative = "greater"` corresponds to r > 0, or a positive correlation;
# `alternative = "less"` would correspond to a negative correlation.

mytest4
# This is basically the same as mytest2, because the correlation is so strong
# that the p-value was already the smallest R can record (< 2.2e-16).

# Palmer Penguins
# Now, try this out for yourself --- load the Palmer Penguins dataset/package
library("palmerpenguins")
glimpse(penguins)

# Pick two measures in the dataset and explore their correlations. You can narrow
# the data to measures from one of the three penguin species in the dataset, or 
# look across all three. Make sure you plot the data and test its normality, to
# confirm that your chosen correlation test(s) are appropriate.

# YOUR CODE HERE

