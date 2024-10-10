# How to do two-way ANOVA and Nested ANOVA
# Casey terHorst, updates by Jeremy Yoder
# 2024.10.08

#-------------------------------------------------------------------------
# TWO-WAY ANOVA

# Let's start with a two-way ANOVA using the snail data 

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data. 
snails <- read.csv("Data/SnailData.csv")
glimpse(snails)

# Let's say we want to know the effects of Location and Tidal Height on 
# snail length

# Two Factor ANOVA (not nested)
model1 <- lm(Length~Location + TidalHeight, data=snails)

anova(model1)

# The code above gives you the main effects of Location and Tidal Height. In a 
# real two-way ANOVA, we'd also like the interaction. In R formula syntax, we
# indicate interactions with : between the factors, like so
model2 <- lm(Length~Location + TidalHeight + Location:TidalHeight, data=snails)
anova(model2)

# or use a shortcut that includes both factors AND their interaction:
model3 <- lm(Length~Location*TidalHeight, data=snails)
anova(model3) # confirm this is the same as above

# Don't forget to check your model assumptions!
plot(model3)
# Looks good

# And then to get the ANOVA table again:
anova(model3)

# Notice that the interaction is very significant. That means that the effect of
# tidal height depends on location. That's super interesting, but if we just 
# looked at the model we ran without the interaction, we would have assumed that
# there's nothing interesting about tidal height or location.
anova(model1)

# Let's make a graph of those data
# I'm going to gather my summary data using emmeans. Using the interaction
# term in this code gives you means for all combinations of factors
library(emmeans)
graphdata <- as.data.frame(emmeans(model3, ~ Location*TidalHeight))
graphdata

ggplot(graphdata, aes(x=Location, y=emmean, fill=factor(TidalHeight))) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9), width=0.8) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1) + # adds error bars
  labs(x="Location", y="Snail Length", fill="TidalHeight") + 
  scale_fill_manual(values=c("Low"="tomato","High"="dodgerblue2")) + 
  theme_bw(base_size=18)
  

#-------------------------------------------------------------------------
# NESTED ANOVA

# Ok, let's switch to nested ANOVA
# We'll use a data set called "schoolteacher" which describes the scores of 
# students in classrooms of different teachers at different schools.

# In this data set, we have student test scores from different schools. The 
# scores come from different teachers in classrooms at each school, and
# "teacher" is our statistical replicate for testing the difference among 
# schools. But we also have replication "within" each teacher! The scores from 
# two different classrooms taught by each teacher. So we can ALSO test the effect 
# of teacher within schools. But in that case, classroom should be the replicate.
# So we need to test two different hypotheses (School, Teacher), but each one 
# uses a different set of replicates, or different error terms.

# However, if we code a Nested ANOVA correctly, it uses the nested term as the 
# error (i.e denominator) to construct an F ratio to test the main factor.

# We'll use 'lmer', in library lme4, which allows you to specify random effects. 
# (Nested factors are always random effects!)

# Import the data. 
teachers <- read.csv("Data/schoolteacher.csv")
glimpse(teachers)

library("lme4")
model4 <- lmer(Score~School + (1|Teacher), data=teachers)

# Note that adding `1|` in front of a factor makes it a random effect. 

# Remember that we need to check the assumptions of our test
plot(model4)
# Homogeneity of variance looks ok

# When we're using lmer, we have to do the QQ plot of residuals ourselves
library(car)
qqp(residuals(model4), "norm")
# Normality looks good too

# Now, we get the stats for the effect of School:
anova(model4)

# As you can see, R doesn't really let us test the significance of random 
# effects, but there's a roundabout way we could do it. We can construct models 
# with and without the random factor and ask which one fits the data better. If 
# the random effect significantly improves the fit, then it is important. We 
# call this a Likelihood Ratio Test, and here's how you do it:

model5 <- lm(Score~School, data=teachers) # model without the random effect

anova(model4, model5)
# This is the Likelihood Ratio Test. It gives AIC values for both models
# and it gives us a p-value for whether one is the better fit. We can
# use that p-value as a hypothesis test for the random effect.

# Finally, if we assumed that both variables were random effects, we could get 
# variance components for both random effects and the error (which is also a 
# random effect)
model6 <- lmer(Score~(1|School) + (1|Teacher), data=teachers)
# You may get a warning message here, but that's ok
summary(model6)

# The variance components are listed under "Random effects" in the output
# If we add up all the numbers in the Variance column, that is the total
# variance. Then divide the variance of each factor by the total variance.
# This gives us the percent of variance explained by each source: 
# Teacher, School, or Residual

# We might have considered School as fixed, and Teacher as random, as above
summary(model4)
# Now we only get variance components for Teacher and Residual. But we can see
# that variance due to Teacher is about 2x greater than the error that is 
# unexplained (residual)

# Now, if we want to graph this data, we need to consider something important.
# Let's say you want to graph the means and se's of the three schools. Remember 
# that the "replicate" for School is "teacher", not every data point. Treating 
# every data point as independent would be pseudoreplication! So we can't just 
# ask for the mean value at each school. Instead, we need to get the means 
# for each teacher, and THEN take the mean of teacher-means within each school.

# A short cut to doing this is to ask for the estimated marginal means from the 
# model, which will give us exactly that info. Note that if you use any of the 
# other models we constructed you will get the wrong answer!
library(emmeans)
graphdata <- as.data.frame(emmeans(model4, ~School))
graphdata

ggplot(data=graphdata, aes(x=School, y=emmean, fill=School)) + 
  geom_bar(fill="DodgerBlue", width=0.5, stat="identity") + 
  guides(fill="none") + 
  ylab("Score") +
  xlab("School") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  theme_bw(base_size=18)

# Note that using the estimated marginal means here is super important for 
# getting the right error bars. Because "Teacher" is the replicate, there are 
# 2 replicates per school. If we tried to get the means without accounting for 
# teacher, we would have pseudoreplicated. The error bars would be smaller, but 
# this represent higher confidence than our data can really support!

# For example:
library(emmeans)
graphdata2 <- as.data.frame(emmeans(model5, ~School))
graphdata2

ggplot(data=graphdata2, aes(x=School, y=emmean, fill=School)) + 
  geom_bar(fill="DodgerBlue", width=0.5, stat="identity") + 
  guides(fill="none") + 
  ylab("Score") +
  xlab("School") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  theme_bw(base_size=18)

# The means are the same as the previous graph, but the error bars are smaller 
# in this new one, because we have misrepresented the replication in the data by
# treating students, rather than teachers, as the unit of replication.
