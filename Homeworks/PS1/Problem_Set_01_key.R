# This is the little bit of R code needed to complete Problem Set 1
# Casey terHorst, revisions by Jeremy Yoder
# 2024.09.18

#-------------------------------------------------------------------------
# Question 1

# Do by hand with calculator


#-------------------------------------------------------------------------
# Question 2

#Clear the environment
rm(list=ls())

library("tidyverse")

# Read the data directly into R (you could also enter it in a csv file)
# When you use 'c', it tells R you're going to give a vector of datapoints
top <- c(3,1,0,5,4,3,6,3,4,7)
bot <- c(3,12,3,4,7,8,7,5,15,9)

# I'm going to now calculate mean, variance, sd, se, and 95% CI of the mean
# For the top ...............
mean(top) # should be 3.6
var(top) # should be 4.5
sd(top) # should be 2.1
sd(top)/sqrt(length(top)) # standard error; should be 0.67
1.96*sd(top) # 95% CI, should be 4.2
qt(0.975, length(top)-1)*sd(top) # OR, technically more correct CI; should be 4.8

# For the bottom ...............
mean(bot) # should be 7.3
var(bot) # should be 15.3
sd(bot) # should be 3.9
sd(bot)/sqrt(length(bot)) # standard error; should be 1.2
1.96*sd(bot) # 95% CI, should be 7.7
qt(0.975, length(bot)-1)*sd(bot) # OR, technically more correct CI; should be 8.9

# Make a graph. Doing this in ggplot takes a bit of setup.
# assemble a data frame in the ggplot "long" format
microcosm <- data.frame(Position=rep(c("top", "bottom"), each=length(top)), Protozoa=c(top,bot))
glimpse(microcosm)

microSumm <- microcosm %>% group_by(Position) %>% 
  summarize(MeanProtozoa=mean(Protozoa), SEM=sd(Protozoa)/sqrt(length(Protozoa)))

microSumm

# now build the figure:
ggplot(microSumm, aes(x=Position, y=MeanProtozoa)) +
  geom_bar(stat="identity", fill="dodgerblue2", position="dodge", size=0.6) + 
  geom_errorbar(aes(ymax=MeanProtozoa+SEM, ymin=MeanProtozoa-SEM), position=position_dodge(0.9), width=0.1)+
  labs(x="Location", y="Number of Protozoa") +
  theme_bw(base_size=18)


#-------------------------------------------------------------------------
# Question 3

# Clear the environment
rm(list=ls())

# Import the data
mydata <- read.csv("Data/kelp bass gonad mass.csv")
glimpse(mydata)

mean(mydata$gonad_mass) # should be 8.2
median(mydata$gonad_mass) # should be 6.4
var(mydata$gonad_mass) # should be 57.9
sd(mydata$gonad_mass) # should be 7.6
sd(mydata$gonad_mass)/mean(mydata$gonad_mass) # CV; should be 0.92

# to get skewness and kurtosis, I use the moments package
library(moments)
skewness(mydata$gonad_mass) # should be 1.4
kurtosis(mydata$gonad_mass) # should be 5.2

# Adding 5 to each value:
mean(mydata$gonad_mass+5) # should be 13.2
median(mydata$gonad_mass+5) # should be 11.4
var(mydata$gonad_mass+5) # should be 57.9
sd(mydata$gonad_mass+5) # should be 7.6
sd(mydata$gonad_mass+5)/mean(mydata$gonad_mass+5) # CV; should be 0.58

skewness(mydata$gonad_mass+5) # should be 1.4
kurtosis(mydata$gonad_mass+5) # should be 5.2

# To add 5 and then multiply by 10:
mean(10*(mydata$gonad_mass+5)) # should be 132.4
median(10*(mydata$gonad_mass+5)) # should be 114.2
var(10*(mydata$gonad_mass+5)) # should be 5793.8
sd(10*(mydata$gonad_mass+5)) # should be 76.1
sd(10*(mydata$gonad_mass+5))/mean(10*(mydata$gonad_mass+5)) # CV; should be 0.58

skewness(10*(mydata$gonad_mass+5)) # should be 1.4
kurtosis(10*(mydata$gonad_mass+5)) # should be 5.2

# To make a histogram:
ggplot(mydata, aes(x=gonad_mass)) + geom_histogram() + theme_bw()

# To convert to z scores:
# Center centers the data on the mean (subtracts mean); Scale divides by s.d.
mydataStnd <- mydata %>% mutate(massStnd = (gonad_mass-mean(gonad_mass))/sd(gonad_mass))
glimpse(mydataStnd)

ggplot(mydataStnd, aes(x=massStnd)) + geom_histogram() + theme_bw()

skewness(mydataStnd$massStnd) # still 1.4
kurtosis(mydataStnd$massStnd) # still 5.2

# To get a probability plot
library(car)
qqp(mydata$gonad_mass, "norm")

#-------------------------------------------------------------------------
# Question 6

# Clear the environment
rm(list=ls())

# Import the data
agaricia <- read.csv("Data/Agaricia.csv")
glimpse(agaricia)

# Are the data normally distributed?
# There are several options:

# Create boxplot for raw data
ggplot(agaricia, aes(x=weight)) + geom_boxplot() + theme_bw()

# If the line is roughly in the middle of the box and the whiskers are about 
# equal, that's a good sign of normality. This is close. Is it close enough?

# Create a histogram (this is more usual)
ggplot(agaricia, aes(x=weight)) + geom_histogram(bins=6) + theme_bw()
# meh, kinda normal-ish? Hard to tell.

# Or just use a probability plot
qqp(agaricia$weight, "norm")
# Yeah, that looks pretty solid

# Log-transform the data anyway to see what happens
# Create boxplot for raw data
ggplot(agaricia, aes(x=log(weight))) + geom_boxplot() + theme_bw()

# Create a histogram (this is more usual)
ggplot(agaricia, aes(x=log(weight))) + geom_histogram(bins=6) + theme_bw()
# meh, kinda normal-ish? Hard to tell.

# Or just use a probability plot
qqp(log(agaricia$weight), "norm")
# original data looks pretty good. Transformation might make it a tiny bit worse.

# Bootstrap our sample
bootmeans <- replicate(1000, { #this tells R I want it to do the same thing 1000 times; open brackets start a function  
  samples<-sample(agaricia$weight,replace=TRUE); 
  mean(samples)  })
# Now you have 1000 different estimates of the mean, based on your 1000 samples
# You can see them by typing
bootmeans
# We can just take the mean of our bootstrapped means to get
mean(bootmeans) 
# The more bootstrapped samples, the closer to the population mean

# You could calculate the confidence interval just as we have before.
# With bootstraps, it's nice because exaclty one of your values represents the 
# 95% confidence limit
# To get confidence interval:
sortedboots <- sort(bootmeans)
#Now our 1000 means are sorted in order. If we'd done 1000 bootstraps, then the 25th value would be the lower CL and the 975th sample would be the lower CL
lowCI <- sortedboots[25]
highCI <- sortedboots[975]
lowCI
highCI

# Makes a histogram of our 1000 bootstraps with CIs marked
ggplot(data.frame(x=sortedboots), aes(x=x)) + 
  geom_histogram(color="white", fill="gray70") +
  geom_vline(xintercept=lowCI, color="blue") +
  geom_vline(xintercept=highCI, color="blue") +
  theme_bw()



