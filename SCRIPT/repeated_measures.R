# How to do "repeated measures ANOVA" with mixed models, and graph results
# Casey terHorst, revisions by Jeremy Yoder
# 2024.10.15

# We'll use a data set collected from plants in two treatments, Control and 
# Fertilized. There were 12 plants in total, 6 in each treatment.
# Root length was measured on the same plants every two weeks until week 10.
# We want to know how the treatment groups differ in root length.

# Note that it's particularly important that you include a column that 
# identifies different plants from each other.

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

# Import the data
rootgrowth <- read.csv("Data/fertdata.csv")
glimpse(rootgrowth)

# Make sure that week, plant, and fertilizer are factors
rootgrowth$week <- as.factor(rootgrowth$week)
rootgrowth$fertilizer <- as.factor(rootgrowth$fertilizer)
rootgrowth$plant <- as.factor(rootgrowth$plant)

# Option 1 for repeated measures --- This approach applies if each subject 
# (each plant in this case) received all of the treatments. This is not true in 
# this study! But here's the code we'd use if it were:
library(lme4)
library(lmerTest)
model1 <- lmer(root ~ fertilizer + (1|plant), data=rootgrowth)
anova(model1)

# It's more common that each subject (plant) received only one treatment and 
# was measured repeatedly. This is true in this study. For this situation,
# we take the following approach:
model2 <- lmer(root ~ fertilizer + week + fertilizer:week + (1|fertilizer/plant), data=rootgrowth)

# We get a warning message the the model failed to converge, but usually this 
# is not a problem, and we'll assume it's okay here.
anova(model2)

# Note that we get a significant fertilizer*week interaction, so we have to stop
# our interpretation there. The interaction means that the effect of the 
# fertilizer treatment depends on which week it was measured in. 

# To plot the effects and see this directly:
library(emmeans)
graphdata <- as.data.frame(emmeans(model2, ~fertilizer:week))
graphdata

ggplot(graphdata, aes(x=week, y=emmean, group=fertilizer, color=fertilizer)) +
  geom_line(aes(linetype=fertilizer)) +
  geom_point()+
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), stat="identity", width=0.1) +
  labs(x="Week", y="root length") + 
  theme_bw()

# Probably the significant interaction effect is driven by the fact the the 
# fertilizer treatment has a bigger effect at week 6 than other weeks. This
# could mean week 6 is a developmental stage at which plants take up more 
# nutrients and put more resources into root growth ... or it could reflect 
# variation in the application of the fertilizer treatment.
