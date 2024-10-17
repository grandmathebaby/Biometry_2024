# Mixed models in R
# Casey terHorst, revisions by Jeremy Yoder
# 2024.10.14

# This script works with data from Mary Alice's Coffroth's lab that examine 
# the growth rate (r) and carrying capacity (K) of different genotypes of 
# coral symbionts, grown in vitro at two different temperatures

# We'll consider Temp as a fixed factor and then try Genotype as both a random 
# and fixed effect


# Clear environment, load the tidyverse
rm(list=ls())
library("tidyverse")

symb <- read.csv("Data/SymbioGrowth.csv")
glimpse(symb)


# We need to make sure that both Temp and Genotype are considered as factors
symb$Temp <- as.factor(symb$Temp)
symb$Genotype <- as.factor(symb$Genotype)


# First, let's try modeling the effects of Temperature and Genotype on r 
# as both fixed effects

model1 <- lm(r~Temp*Genotype, data=symb)
plot(model1) # Check assumptions

library(car)
qqp(residuals(model1), "norm")

# This all looks pretty good!

# Run ANOVA on that model
anova(model1)

# or we could have used capital A Anova and found the same thing
Anova(model1, type="III")  # this also requires library(car). 

# This is useful if you ever want to use Type II or Type III sums of squares. 
# Type II is the default for Anova() and type III is the default if you use 
# anova() with the lmerTest library

# If you wanted to do a post-hoc test
library(emmeans)
emmeans(model1, pairwise~Temp*Genotype, adjust="tukey")

# If I want to use the agricolae Tukey function for group-label letters, I have
# to futz with things a little 
tx <- with(symb, interaction(Temp, Genotype))
rmod <- lm(r~tx, data=symb)
library(agricolae)
HSD.test(rmod, "tx", console=TRUE)

# Ok, now instead let's model Genotype as a random effect. We have to use lmer() 
# now because lm() can't handle random effects. There are also other commands 
# in other libraries that handle random effects, such as lme() or nlme, but the 
# syntax is different, so I'm going to stick with lmer()

library(lme4)
library(lmerTest) # We need this to get anova results from lmer models

# If we JUST wanted to model Genotype as a random effect and ignore Temperature
modelG <- lmer(r~(1|Genotype), data=symb)
summary(modelG) #We can use summary to get variance components

# Let's add Temperature as a fixed effect, interacting with Genotype
modelGxT <- lmer(r~Temp + (1|Genotype) + (1|Genotype:Temp), data=symb)
# You might see a "singular fit" warning here. Ignore that for now. 
# You can still proceed.
summary(modelGxT)
anova(modelGxT)

# Note that R will only test the significance of fixed effects
# If we really want to test the significance of a random effect, then we need 
# to use a likelihood ratio test by comparing models with and without the 
# random effect.

# One way to do this:
modelT <- lm(r~Temp, data=symb) # Model with only temperature
anova(modelGxT, modelT) # LRT to determine effect of Genotype

# This shows that the model fit is significantly better with the random effect
# added (model GxT); you might also compare a model without the interaction.
modelGT <- lmer(r~Temp + (1|Genotype), data=symb)
anova(modelGxT, modelGT, modelT)

# This shows that the full model with random effect of genotype and 
# genotype:temperature interaction is best fit, with a strongly significant LRT
# test (X^2 > 0) and difference in AIC more than 20 units from the next-best ---
# so a much better fit.

# Graphs
# If we want to graph the effects of Temperature when Genotype is a random 
# effect, then it's particularly important that we don't use the raw data for 
# the graph. Rather, we get the estimated marginal means, which will give us the
# mean and SE from the model, accounting for the random effect.

library(emmeans)
graphdata <- as.data.frame(emmeans(modelGxT, ~Temp))
graphdata

# BarPlot of growth rates
ggplot(graphdata, aes(x=Temp, y=emmean, fill=as.factor(Temp))) + 
  geom_bar(stat="identity", position="dodge", linewidth=0.6) + 
  # ^ determines the bar parameters, including width
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  # ^ adds error bars
  labs(x="Temperature", y="growth rate (r)") + 
  # ^ labels the x and y axes
  scale_fill_manual(values=c("26"="dodgerblue2","30"="red3"), name="Temp") +  
  # ^ fill colors for the bars
  theme_bw(base_size=14) 

### Try it yourself

# Remember the data on floral visitor counts for flowering species with 
# with different kinds of floral symmetry? This is another case where a random
# effect makes sense: Floral visits are observed in different communities
# which may have very different diversity of possible visitors. We can analyze
# these data with community identity as a random effect, and floral symmetry 
# as a fixed effect.

# The floral visitors data is in "degree_per_plant.csv". Note that to work
# with this, you'll need to filter out a small subset of observations in which
# flowers are recorded as having 0 visitors --- and you may need to transform 
# the data to better conform to normality. Fit models with and without a random
# effect of community (the "matrix" column in the data table), and test whether
# the random effect is informative. Then plot the marginal means for species 
# with different floral symmetry to illustrate the effect of symmetry on floral
# visitor diversity in the best-fit model.

degrees <- read.csv("Data/degree_per_plant.csv") %>% filter(N.poll>0) %>% 
  mutate(logPoll = log(N.poll), matrix=factor(matrix)) %>% rename(community=matrix)

glimpse(degrees)

