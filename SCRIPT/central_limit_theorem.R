# The Central Limit Theorem, a practical demonstration
# Jeremy Yoder
# last revised 2024.09.11

# SETUP
# clear the active memory
rm(list=ls())

# load the tidyverse
library(tidyverse)
library(moments)

# Read in our data
# These are statistics describing species visiting flowers for a large number 
# of flowering plant species in many different communities around the world, 
# from the supporting data for 
#   Yoder JB, G Gomez, and CJ Carlson. 2020. Zygomorphic flowers have fewer 
#   potential pollinators. Biology Letters. 16(9): 20200307. 
#   doi.org/10.1098/rsbl.2020.0307

visitors <- read.csv("Data/flower_visitors.csv")

glimpse(visitors)

# There's a BUNCH of data here but let's look at the number of species visiting
# each plant species flowers, `n.poll`

ggplot(visitors, aes(x=n.poll)) + geom_histogram(fill="#cab2d6") + 
  labs(x="Visitor species", y="Plant species") +
  theme_bw()

# That looks very non-normal! What do the diagnostics say?
skewness(visitors$n.poll)
kurtosis(visitors$n.poll)

# The Central Limit Theorem holds that if we take repeated samples from ANY
# distribution, the mean values for those repeated samples will be normally
# distributed --- if the samples are big enough. 

# Let's see that in action with our badly skewed flower-visitor data.

# Suppose we just take a sample from our larger data set. The sample() function
# draws X random values from a vector of values, without replacement (by 
# default). Let's try samples of 10, 50, and 500 values:

rand10 <- sample(visitors$n.poll, 10)
rand50 <- sample(visitors$n.poll, 50)
rand500 <- sample(visitors$n.poll, 500)

# These samples are just vectors, so to plot them quickly use the base R hist()
# function:

hist(rand10)
hist(rand50)
hist(rand500)

# You can see that the samples reflect the strong skew of the population from
# which their drawn, and only reflect it better as the sample gets bigger. But
# the CLT is about the distribution of MEAN values for REPEATED samples. So
# we actually need to take a bunch of samples of different sizes, and look at
# the distribution of the sample mean values for different sample sizes.

# To take repeated samples we'll need a bit of programming. A for() loop tells
# R to repeat a particular set of actions while counting through a variable
# that keeps track of the repetitions.

# Start by taking samples of 10 values
# We'll make an empty vector to hold our sample means:
samp10 <- vector(mode="numeric", length=1000)

# Then run a for() loop to take the means of 100 samples of 10 values
for(r in 1:1000){
 samp10[r] <- mean(sample(visitors$n.poll, 10)) 
}

ggplot(data.frame(MnNpoll=samp10), aes(x=MnNpoll)) + 
  geom_histogram(fill="#fdbf6f") + theme_bw()

# Well that doesn't look much more normal than the original population of data
# values. What about a sample of 50?

samp50 <- vector(mode="numeric", length=1000)

for(r in 1:1000){
  samp50[r] <- mean(sample(visitors$n.poll, 50)) 
}

ggplot(data.frame(MnNpoll=samp50), aes(x=MnNpoll)) + 
  geom_histogram(fill="#ff7f00") + theme_bw()

# That's better! Let's go bigger
samp500 <- vector(mode="numeric", length=1000)

for(r in 1:1000){
  samp500[r] <- mean(sample(visitors$n.poll, 500)) 
}

ggplot(data.frame(MnNpoll=samp500), aes(x=MnNpoll)) + 
  geom_histogram(fill="#fb9a99") + theme_bw()

# What do our diagnostic statistics say?
skewness(samp500)
kurtosis(samp500)

# What do you suppose happens if we take samples that are as large as the 
# population we're sampling from? (Note that we'd need to do this with 
# replacement, to avoid getting 1000 identical mean values.)
