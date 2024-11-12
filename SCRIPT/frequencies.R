# How to analyze frequencies
# Casey terHorst, revisions by Jeremy Yoder
# 2024.11.05


# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Chi-square and G-tests
# Let's say we looked at frequency of feeding in some mice. We watched each for 
# an hour and for each animal, we recorded whether they ate or not.

# In total, we observed 600 mice
ate <- 375
noeat <- 225

# Is this different than what we'd expect by random chance?

# Set up our table of observed and expected values. First, the observed values:
observed <- matrix(c(ate, noeat), nrow=1, ncol=2)

# You will need to define the matrix by the number of rows and columns.
# In order, you list the items in column 1, then the items in column 2, etc.

# Then, the expected values
expected <- matrix(c((ate+noeat)/2, (ate+noeat)/2), nrow=1, ncol=2)

# The expectation is that, if mice make a decision to eat at random during the 
# one-hour observation window, then they have a 50% chance of eating and a 50%
# chance of not eating while you were watching. So this takes our total number
# of observations (600) and divides it by 2 --- we expect to see equal numbers
# of mice performing each activity. We'd do this differently if there were, say,
# 3 or 4 activities that mice might choose among, or if we have a different
# random expectation, like the Mendelian ratios in random mating of diploid
# genotypes.

# With both our observations and our expectation, we calculate G:
Gvalue <- 2*sum(observed*log(observed/expected))
Gvalue 

# To get a p-value, we compare our observed value of G to a Chi-squared 
# distribution with one degree of freedom (df = number of categories - 1)
1-pchisq(Gvalue, df=1) 

# Or an alternate way to do the same thing:
library(DescTools)
GTest(observed, p=c(0.5, 0.5))
# p is a list of expected probabilities in each group. Here again, we 
# expected 50% in each group. But this would be a different vector if we had
# differing probabilties expected and/or more groups.

# Or to do X2 test:
X2test <- sum((observed-exp)^2/exp)
X2test
1-pchisq(X2test, df=1)

# How to do log linear analysis

# Clear the environment
rm(list=ls())

# Imagine an experiment in which we had control and experimental plants. We also 
# did our experiment in four different seasons. At the end of the experiment, we 
# measured the frequency of plants in each plot that had either set fruit or not
# set fruit. To set up the data, we need to make a dataframe, just like we 
# always do. You could do this in Excel and we'd have one column for Season 
# (with four different seasons), one column for Treatment (exp or control), one 
# column for FruitSet (yes or no), and column for Frequency, which is the number
# that corresponds to each combination.

fruitset <- read.csv("Data/Companion.csv")
glimpse(fruitset)

# We're interested in the main effects season and our experimental treatment, 
# and whether they interact. We can address this by fitting a generalized linear
# model with Poisson-distributed error ...

library(lme4)

model1 <- glm(Frequency~Season:Fruit + Treatment:Fruit + Season:Treatment:Fruit, family=poisson, data=fruitset)

# And then use a modified ANOVA, testing against a chi-squared distribution:
anova(model1, test="Chisq")

# In this output, we're interested in the effect of Season, which is the 
# Season:Fruit interaction, and the effect of Treatment, which is 
# Treatment:Fruit, and the interactions between Season and Treatment, 
# which is Season:Treatment:Fruit.


