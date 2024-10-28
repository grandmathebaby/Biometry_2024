# Power analysis in R
# Created by Jeremy Yoder
# 2024.10.23

# clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Power refers to the probability that a particular statistical test will find
# a significant TRUE effect. It depends on the size of the effect you expect to
# find, the variation around the effect, AND the size of the sample you use to 
# test for the effect. 

# Imagine you're doing a one-sample t-test. This tests the hypothesis that your
# sample mean differs from some a priori value, often just zero. The null 
# distribution for the test defines a minimum value of the t-statistic (your 
# sample mean divided by the standard error) at which there is a probability 
# less than alpha (usually less than 0.05) that you could obtain a given t-
# statistic from a population with a true mean equal to zero. That minimum value
# is your critical t-statistic. 

# Because the real world is variable, a sample drawn from a population with a 
# true difference from your null may, by chance, still have a t-statistic that 
# falls below the critical t-statistic, if the variation around your observed 
# t-statistic is wide enough. That variation is a function of your confidence
# interval, such that a bigger sample size, which means a smaller CI, will reduce
# the probability that you could get an observed t-statistic below the critical
# value from a sample that really does differ from the null.

# Power analysis attempts to evaluate these factors, which all depend on (1) the
# statistical test you're planning to use, (2) the effect size you expect to see,
# and (3) the sample size, and therefore the CI around the expected effect size.
# In practice, you can use power analysis to determine whether your experimental
# plan will give you a big enough sample size to detect the effect you expect.
# If it won't, you can (hopefully) plan to increase sample size, or otherwise
# re-design your experiment, until you do have sufficient power.

# We'll use functions found in the `pwr` package, which provides options for a 
# number of GLM-type tests.
library("pwr")

# Consider a two-sample t-test. Let's say you want to test the effectiveness of
# a herbicide in keeping broadleaf weeds from establishing in campus lawns. You
# lay out 0.5-meter^2 survey quadrats across the library quad, and apply 
# herbicide to half of them. A month later you survey the percent cover of weeds
# in each quadrat, and compare weed cover in the treated quadrats to the 
# untreated ones. 

# You know, before starting, that the lawn has an average of 20% weed cover, and
# you hypothesize that the herbicide will bring this down to 5%. You find that 
# variation among quadrats on untreated lawn means the standard deviation of 
# weed cover is 2.5%. In this context, your effect size is (5-20)/(2.5) = -6.
# How many quadrats would you have to survey to be sure to detect an effect that
# big? Is five of each treatment (herbicide or control) enough?

pwr.t.test(n=5, d=-6, sig.level=0.05, type="two.sample")

# According to this, yes! You'd have power = 1, or 100% probability of detecting
# that effect with 5 quadrats in each treatment. What if the effect were smaller?
# Let's say the herbicide only cuts weed cover to 10%. That would be an
# effect of (10-20)/2.5 = -4:

pwr.t.test(n=5, d=-4, sig.level=0.05, type="two.sample")

# You've still got very good power, but not as much. What if the herbicide only
# cuts weed cover to 15%?

pwr.t.test(n=5, d=-2, sig.level=0.05, type="two.sample")

# Now power is reduced to 0.79 --- you have a more than 20% chance of failing to
# find a real effect this small, in a sample of 5 quadrats in each treatment.
# But you might be able to add quadrats! Try boosting the sample size:

pwr.t.test(n=10, d=-2, sig.level=0.05, type="two.sample")

# Doubling the sample size increases your power to find a smaller effect.

# The functions in `pwr` also let you determine other variables in the set
# of sample size, effect size, significance level, and power. If we run the
# `pwr.t.test()` function as we have above, but leave out effect --- and instead
# set power = 0.95, meaning we would reliably identify a true effect 95% of the
# time --- we can get the MINIMUM effect we'd be able to detect with that power
# or better for a given sample size and significance level:

pwr.t.test(n=5, sig.level=0.05, power=0.95, type="two.sample")

# The `pwr` package offers similar functions for a number of other common tests.
# These have their own effect sizes, however, which can be complicated. For 
# instance, a one-way ANOVA with fully balanced sampling (equal n per group) has
# an effect modeled as f, roughly equal to the square root of the sum of squares,
# which incorporates variation across the full dataset and within groups defined
# by the single predictor factor.

# An f-value of 0.25 is considered a "medium" effect, and here's how it plays
# out in the one-way ANOVA power test for a TOTAL sample of 20 observations in
# 4 treatment groups:

pwr.anova.test(k=4, n=20, f=0.25, sig.level=0.05)

# We only have 42% probability of correctly identifying a true "medium" effect!
# How big would the sample size need to be to boost power to 0.95 for an effect
# of this size? Use the `pwr.anova.test()` function to calculate it:

# YOUR CODE HERE

# The base-R package `stats` has an alternative anova power test, that takes
# number of groups, n per group, and between- and within-group variance, rather
# than a formal effect size. 

power.anova.test(groups=4, n=5, between.var=25, within.var=100, sig.level=0.05)

# The `pwr` package also has a function for power in correlation tests --- and, 
# conveniently, it uses the correlation coefficient itself, r, 
# as the effect size:

pwr.r.test(10, r=0.1, sig.level=0.05, alternative="two.sided")

# We have VERY low power to find a correlation of 0.1 with only 10 observations.
# What sample size would we need for 95% power? Use the `pwr.r.test()` function
# to find it:

# YOUR CODE HERE

# Rather than plugging in individual sets of options and getting back one value,
# we can also use these functions to compare how power changes with sample or
# effect size. Let's try that out with some R code, to look at power to detect
# correlations, again:

# First, define a range of correlations we expect to see:
rs <- seq(0.1, 0.5, by=0.01)

# And the range of power that interests us:
powers <- seq(0.5, 0.9, by=0.1)

# We can set up a data frame with each combination of r and power this way:
CorPower <- expand.grid(r=rs, power=powers) 
glimpse(CorPower)

# We'll assume we use alpha=0.05 in all tests, and then we can calculate the
# sample size we'd need to detect each correlation value with a given power. 
# We're using a very useful R function called `mapply()` (for 'multivariate 
# apply') to run the `pwr.r.test()` function on every combination of correlation
# effect and power in the CorPower dataframe:

CorPower$sample_n <- mapply(
  function(r,p) floor(pwr.r.test(r=r, sig.level=0.05, power=p)$n), 
  CorPower$r, CorPower$pow
  )

# (The `floor()` function is there to round estimated sample sizes down to the
# nearest integer --- otherwise the function will return fractional n values,
# which doesn't make much sense.)

glimpse(CorPower) # And here's our data

# Now let's visualize it:

ggplot(CorPower, aes(x=r, y=sample_n, color=power, group=factor(power))) + 
  geom_line() + 
  scale_color_gradient(low="#a6bddb", high="#014636", name="Power") + 
  labs(x="Correlation coefficient", y="Sample size") +
  theme_bw() + theme(legend.position=c(0.85,0.75))

# You should see that, in general, smaller samples are needed to detect larger
# correlations, and that this contrast is stronger when you increase the 
# power desired. That is, it takes a bigger sample to detect a given 
# correlation with 90% power than it does to detect the same correlation with
# 70% power. 

# This kind of analysis, looking at the relationships between power, sample 
# size, and effect size, can help define your options for planning an 
# experiment. It's probably impractical to hope for 90% power to detect a 
# correlation of 0.1 --- but smaller samples may still have good power to 
# detect biologically relevant correlations.

# Details on the functions in the `pwr` package, and the formulas for the effect
# sizes they work with, are at https://www.statmethods.net/stats/power.html

