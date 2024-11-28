# Bayesian regression to deal with weird data distributions
# Jeremy Yoder, 2024.11.25

# Almost all of the work we've done this semester is in a "frequentist"
# framework, which assesses the probability of a dataset relative to a null
# distribution derived from first-principles understanding of probability. That
# approach can get us a long way, but it has some notable limitations --- 
# frequentist methods rarely let us incorporate information we know about a 
# situation before we conduct an experiment, and the theory underlying 
# frequentist testing often over-simplifies complex biological systems. 

# In the last few decades, biologists have increasingly adopted Bayesian 
# approaches, which can accommodate more complex "prior" understandings of the 
# world --- but which are also more computationally complex, and often require 
# you to make explicit choices about those priors. Bayesian approaches are 
# popular in phylogenetics and population genetics, but we can also get a taste
# of this philosophical choice in "simple" linear regression.

# Here, we'll return to our example data set on plants' floral symmetry, floral
# visitor diversity, and other ecological factors (from Yoder et al 2020,
# Biology Letters). The visitor species count data are highly non-normal --- 
# and because the analysis involves a comparison among species, we really should
# account for the counfounding effect of phylogenetic relationships among those
# species. The solution Yoder et al landed on was Bayesian regression, using 
# the `brms` package in R.

# Clear the environment and load the tidyverse
rm(list=ls()) 
library("tidyverse")

# Load the necessary packages 
library(fitdistrplus) # Useful for describing the distribution of data
library(brms) # For running Bayesian regression

# Load the flower visitor data
visitors <- read.csv("Data/flower_visitors_all.csv")
glimpse(visitors)

# This is a much more complete version of this data than we've looked at up to
# now --- it includes a bunch of different measures of pollinator diversity and
# pollinator sharing, and loadings on four PC axes derived from phylogenetic
# relationships among the plant species in the data set, AND the latitude of the 
# community in which the plant species was observed. The latitude is relevant
# because species diversity is higher in the tropics, so we may want to include
# it as a covariate in any model involving species diversity, like the visitor
# species counts. Column names with the `.sc` suffix have been rescaled for 
# convenience; we'll use the scaled latitude and PC axes.

hist(visitors$n.poll)

# First, let's visualize our predictors, just to get a sense of what's going on:

# effect of floral symmetry?
ggplot(visitors, aes(x=symmetry, y=log10(n.poll))) + geom_boxplot()

# effect of latitude?
ggplot(visitors, aes(x=lat.sc, y=log10(n.poll))) + geom_point() + geom_smooth(method="lm")

# effect of phylogeny?
ggplot(visitors, aes(x=pc1.sc, y=log10(n.poll))) + geom_point() + geom_smooth(method="lm")
ggplot(visitors, aes(x=pc2.sc, y=log10(n.poll))) + geom_point() + geom_smooth(method="lm")

# Looks like many of these have some potential contribution, but we'll need to
# test them formally, ideally all together.

# As we've seen before, visitor species number is highly right-skewed:
hist(visitors$n.poll)

# The `descdist()` function gives us a visual assessment of distributions that
# might be a decent fit for this data, by bootstrapping the data:
descdist(visitors$n.poll, boot=100)
# Looks like beta or gamma will be pretty good.

# Now we'll use `brms` to fit some candidate models. For all models, we want to
# use a random effect of community ID (in this version of the data, the `web` 
# column) to control for the fact that each community has different baseline 
# diversity of plants and flower-visiting animals.

# We'll fit three models as a trial. For each of these, the `brm()` function 
# essentially creates a little C++ program to simulate data as part of fitting
# the model --- this is where priors come in. We mostly won't mess with the 
# default priors, but we do need to specify a distribution for the data, which 
# we do in a very similar way to GLMs. In this case, we'll use a gamma 
# distribution. The `iter` option specifies how long to run the simulation, and 
# the `cores` option tells `brm` to spread the work of simulating over four 
# independent processor cores, which speeds things up. 

# For a first model, we'll fit only the random effect of community ID. This may
# take substantial processing time, but you'll get progress readouts to track
# the simulations.
npoll.M <- brm(n.poll ~ (1|web), data=visitors, family="gamma", iter=5000, cores=4)

# Now let's fit more complete models, accounting for effects of floral symmetry,
# latitude, and phylogeny. We'll try with and without an interaction between 
# symmetry and latitude (maybe the effect of symmetry differs in the tropics?), 
npoll.MSLP <- brm(n.poll ~ symmetry + lat.sc + pc1.sc + pc2.sc + (1|web), data=visitors, family="gamma", iter=5000, cores=4)
npoll.MSxLP <- brm(n.poll ~ symmetry * lat.sc + pc1.sc + pc2.sc + (1|web), data=visitors, family="gamma", iter=5000, cores=4)

# To compare models fitted in this way, we use a procedure called "leave one 
# out" (LOO) cross-validation. This is (very roughly) analogous to comparing AIC
# scores for alternative models ---  
npoll.comp <- LOO(npoll.M, npoll.MSLP, npoll.MSxLP) 

npoll.comp 
# Our best-fit is MSxLP, which includes the interaction of symmetry and latitude

# We can get an estimate of the variation explained using this function:
bayes_R2(npoll.MSxLP) # rsq = 0.28

# This is a point at which Bayesian inference clearly differs from frequentist
# methods. Rather than assessing whether estimated effects differ from zero (are
# "significant") by conducting a t-test of the estimate, or assessing whether
# a predictor explains significant variation using an F-test, we look at the 
# posterior distribution of estimates from the simulation underlying the model.

summary(npoll.MSxLP)

# Each parameter estimate is accompanied by the 95% CI derived from the 
# simulation procedure --- an effect is "significant" in this framework if the
# 95% CI doesn't cross zero. You can see that there's a signficant negative
# effect of zygomorphic floral symmetry, and a significant positive effect of
# latitude, for instance.

# If we plot the model, we can get visualizations of the posterior parameter 
# distributions overall, and in time-series
plot(npoll.MSxLP)


