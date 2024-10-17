# This is the little bit of R code needed to complete Problem Set 1
# Casey terHorst, revisions by Jeremy Yoder
# 2024.09.18

# Clear the environment, load the tidyverse
rm(list=ls())

library("tidyverse")

#-------------------------------------------------------------------------
# Question 1

# create vectors with the data from the question:
flat <- c(3, 3, 4, 5, 2, 3, 2, 3, 4, 5)
comp <- c(3, 5, 2, 1, 7, 8, 7, 4, 11, 9)

# calculate means and sds
mean(flat) # should be 3.4
sd(flat) # should be 1.1

mean(comp) # should be 5.7
sd(comp) # should be 3.2

# two-sample t-test with pooled/equal variances
t.test(flat, comp, var.equal=TRUE) # t = -2.1, p < 0.047
# so, significantly different

# ... and with separate variances (the default)
t.test(flat, comp) # t = -2.1, p = 0.056
# so, NOT significantly different

# Hmm, different answers. Well, are the variances really equal?
var(flat)
var(comp)

# And now to make a graph. First, assemble a dataframe
urchins <- data.frame(
                    site=rep(c("flat","complex"), each=length(flat)),
                    N_urchins=c(flat, comp)
                    )
glimpse(urchins)

# You could use a boxplot:
ggplot(urchins, aes(x=site, y=N_urchins)) + geom_boxplot() + theme_bw()

# Or you can make a bar plot, with summarized data:
urchSumm <- urchins %>% group_by(site) %>% 
  summarize(meanN=mean(N_urchins), SEM=sd(N_urchins)/sqrt(length(N_urchins)))

urchSumm

ggplot(urchSumm, aes(x=site, y=meanN)) +
  geom_bar(stat="identity", fill="dodgerblue2", size=0.6) +
  geom_errorbar(aes(ymax=meanN+SEM, ymin=meanN-SEM), width=0.1) +
  labs(x="Site", y="Urchins (#cm^2)") + 
  theme_bw()


#-------------------------------------------------------------------------
# Question 2

# Import the data
butterflies <- read.csv("Data/CoastBuckwheat.csv")
glimpse(butterflies)

# Are the data normally distributed?
library(car)
qqp(butterflies$Density, "norm")
# This looks pretty good

# Do a one-sample t-test to see if the mean is significantly different from 4
t.test(butterflies$Density, mu=4, na.rm=TRUE)
# t = 2.7, p = 0.01

# And let's make a graph; we'll need to do some data-munging first
butterSumm <- butterflies %>% summarize(mnDensity=mean(Density), SEM=sd(Density)/sqrt(length(Density)))
butterSumm

ggplot(butterSumm, aes(y=mnDensity, x=0)) + 
  geom_bar(stat="identity", fill="dodgerblue", width=0.5) +
  geom_errorbar(aes(ymin=mnDensity-SEM, ymax=mnDensity+SEM), width=0.1) +
  geom_hline(yintercept=4, linetype=2) +
  xlim(-1,1) +
  labs(y="Mean plant density per plot") +
  theme_bw(base_size=18) + theme(axis.text.x=element_blank(), axis.title.x=element_blank())

# a histogram with a line at 4 is arguably better here
ggplot(butterflies, aes(x=Density)) + 
  geom_histogram(bins=6, fill="dodgerblue") +
  geom_vline(xintercept=4, linetype=3) +
  labs(x="Buckwheat Density (# per 25m^2)") +
  theme_bw()


#-------------------------------------------------------------------------
# Question 3

#Import the data
ade <- read.csv("Data/RunTimes.csv")
glimpse(ade)

# This data is in wide format and it's generally easier to work with it in 
# long format, so I'm going to convert it:
adeLng <- pivot_longer(ade, cols=c("Water", "Sportsdrink"), names_to="Treatment", values_to="time_sec")
glimpse(adeLng)

# To test normality
library(car)
qqp(adeLng$run_time_s, "norm")
# great!

# Now let's do a paired t-test
t.test(adeLng$time_sec[adeLng$Treatment=="Water"], 
       adeLng$time_sec[adeLng$Treatment=="Sportsdrink"], 
       paired=TRUE
       )

# To see the direction and size of the effect:
ggplot(adeLng, aes(x=Treatment, y=time_sec)) + geom_boxplot() + theme_bw()
# There is a significant difference, but the effect is pretty small!

#-------------------------------------------------------------------------
# Question 4

# Import the data
babies <- read.csv("Data/crying_babies.csv")
glimpse(babies)

# First make a scatterplot
ggplot(babies, aes(x=IQ, y=cryduration)) + geom_point() + theme_bw()

# Then do each type of correlation test
cor.test(babies$cryduration, babies$IQ, method="pearson")
# r = 0.57, p = 0.006

cor.test(babies$cryduration, babies$IQ, method="spearman")
# rho = 0.39, p = 0.07

cor.test(babies$cryduration, babies$IQ, method="kendall")
# tau = 0.25, p = 0.11

# Pearson's is the more powerful test, but it also requires normality for both
# variables under consideration
library(car)
qqp(babies$cryduration, "norm")
qqp(babies$IQ, "norm")

# These both look mostly normal, so we should use Pearson's. But there are a 
# few points that fall outside the confidence limits, so Spearman's is a safe
# (conservative) choice.

#-------------------------------------------------------------------------
# Question 5

# Import the data
ballots <- read.csv("Data/butterflyballot.csv")
glimpse(ballots)

# We want to first create a second data set without the last data point (Palm Beach County)
NoPalmBeach <- ballots[-67,]
glimpse(NoPalmBeach)

# First, let's look at the plot to see if the data look linear
ggplot(NoPalmBeach, aes(x=Bush, y=Buchanan)) + geom_point() + theme_bw()

# Pretty linear. Are the data normal?
library(car)
qqp(NoPalmBeach$Bush, "norm")  #no!
qqp(NoPalmBeach$Buchanan, "norm") #no!

# That means we should either use a Spearman correlation or transform the data. 
# I'm going to go with Spearman's here
cor.test(NoPalmBeach$Bush, NoPalmBeach$Buchanan, method="spearman")
# rho = 0.94, p < 2.2e-16

# If we could log transform our data and it's normal, use Pearson's correlation
NoPalmBeach <- NoPalmBeach %>% mutate(logBush = log(Bush), logBuch = log(Buchanan))

qqp(NoPalmBeach$logBush, "norm")
qqp(NoPalmBeach$logBuch, "norm")
# much more normal!

cor.test(NoPalmBeach$logBush, NoPalmBeach$logBuch, method="pearson")
# cor = 0.93, p < 2.2e-16

# No matter what method we use, we get a strongly significant, 
# positive correlation

# Now let's look at the data with Palm Beach County included
ballots <- ballots %>% mutate(logBush = log(Bush), logBuch = log(Buchanan))

ggplot(ballots, aes(x=Bush, y=Buchanan)) + geom_point() + theme_bw()
# The outlier is pretty obvious

# Let's do a formal outlier test
model1 <- lm(Bush~Buchanan, data=ballots)
outlierTest(model1)

# It looks like Palm Beach County is a clear outlier. That doesn't mean we 
# throw it out, but we might ask why. It seems like MANY more people voted for 
# Buchanan relative to the number who voted for Bush there. But note that our 
# test also found a second outlier, so we might want to investigate that too
# before we draw too many conclusions about the Palm County outlier. 

#-------------------------------------------------------------------------
# Question 6

# See Excel spreadsheet


#-------------------------------------------------------------------------
# Question 7

# Import the data
streams <- read.csv("Data/streams.csv")
glimpse(streams)

ggplot(streams, aes(x=Biomass, y=NumberSpp)) + 
  geom_point(color="seagreen") + 
  theme_bw()

# Before we can fit a regression line, we need to describe a model 
# for this relationship

fit1 <- lm(NumberSpp~Biomass, data=stream)

ggplot(streams, aes(x=Biomass, y=NumberSpp)) + 
  geom_point(color="seagreen") + 
  geom_abline(intercept=coef(fit1)[1], slope=coef(fit1)[2]) +
  theme_bw()

# Hmm, doesn't look linear

# Does this model meet the assumptions of linear regression?
# Well, first of all, this doesn't look like a linear relationship
# So let's try ln-transformation of biomass, as it is log-normally distributed
streams <- streams %>% mutate(lnBiomass = log(Biomass))

fit2 <- lm(NumberSpp~lnBiomass, data=streams)

ggplot(streams, aes(x=lnBiomass, y=NumberSpp)) + 
  geom_point(color="seagreen") + 
  geom_abline(intercept=coef(fit2)[1], slope=coef(fit2)[2]) +
  theme_bw()

# That looks much better!

# Ok, now let's use the residuals to check the rest of our assumptions
residuals2 <- residuals(fit2)

qqp(residuals2, "norm")

# They're pretty much in the bounds of what we'd expect for normality

# Ok, so what about homogeneity of variance? Let's plot the residuals against 
# our fitted values

fitted2 <- fitted(fit2)

ggplot(data.frame(resid=residuals2, fit=fitted2), aes(x=fit, y=resid)) +
  geom_point(color="seagreen1") +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()

# This looks good because the data appear evenly distributed both vertically 
# and horizontally

# Here's a shortcut to getting all of the diagnostic plots you need
plot(fit2)

# With the leverage plot, you want values Cook's distance less than 1:
plot(fit2,4)

# Finally, to test whether this model explains more variation than expected
# by chance:
summary(fit2)

#-------------------------------------------------------------------------
# Question 8

# Import the data, which I have put in a csv file named "algae"
algae <- read.csv("Data/algae.csv")
glimpse(algae)

fit1 <- lm(Surface_area~Height, data=algae)

ggplot(algae, aes(x=Height, y=Surface_area)) +
  geom_point(color="darkorange") +
  geom_abline(intercept=coef(fit1)[1], slope=coef(fit1)[2]) +
  theme_bw()


# Let's test assumptions
plot(fit1)

algae$resid <- residuals(fit1)

ggplot(algae, aes(x=Height, y=resid)) +
  geom_point(color="darkorange4") +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()

# kinda looks like much more variance on the right side than the left
qqPlot(fit1) # and some normality issues

# Try ln-transformation of both variables
algae <- algae %>% mutate(lnHeight=log(Height), lnSA=log(Surface_area))

fit2 <- lm(lnSA~lnHeight, data=algae)

plot(fit2)

algae$resid2<-residuals(fit2)

ggplot(algae, aes(x=lnHeight, y=resid2)) +
  geom_point(color="darkorange3") +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()

qqp(algae$resid2, "norm") # pretty normal

# to get r^2 for the model and statistics
summary(fit2)
plot(lnSA~lnHeight, data=algae)
abline(fit2)

# Check for outliers:
outlierTest(fit2) #no outliers

# Check for high leverage points
plot(fit2,4)

# Make a better plot with confidence limits in ggplot
ggplot(algae, aes(x=lnHeight, y=lnSA)) +
  geom_point(color="darkorange") +
  labs(x = "ln(Height)", y="ln(Surface Area)") +
  geom_smooth(method="lm", formula=y~x, color="white") +
  theme_bw()

# Because we're particularly interested in the value of the slope here,
# and because there is error in the measure of x and y, we really should use a 
# model II regression.
library(lmodel2)

model1 <- lmodel2(lnSA~lnHeight, range.y="relative", range.x="relative", data=algae, nperm=99)
model1 # Use these parameters (from RMA) to get estimates of slope and intercept

# to plot from those parameters
ggplot(algae, aes(x=lnHeight, y=lnSA)) +
  geom_point(color="darkorange") +
  labs(x = "ln(Height)", y="ln(Surface Area)") +
  geom_abline(intercept=2.13, slope=1.17, color="darkorange") +
  theme_bw()


