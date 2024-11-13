# Multivariate Analyses
# PCA: Principal Components Analysis
# Casey terHorst, revisions by Jeremy Yoder
# 19 Nov 2023

# Clears the environment and load the tidyverse
rm(list=ls()) 
library("tidyverse")

# Andrea measured 38 morphological variables in many different species of 
# spiders. She has grouped those species together into ecological guilds: Web 
# Builders, Fishers, Foliage Runners, Jumpers, and Wanderers. She wants to know 
# whether traits differ between guilds. The 38 different traits are standardized 
# by total body mass. In the data file, she has renamed these traits a, b, c, 
# etc. I'd normally advise against this because you're going to want those trait
# names in R at some point! However, for now, it means less typing for us.

# Import the Data
spiders <- read.csv("Data/spiders.csv")
view(spiders)

# Before we can use this data file, R wants ONLY trait data in order to do the 
# PCA. In other words, we need to get rid of the Guild column in our data set 
# (just to get PCA scores). Since that's the the first column, we do this:

spiders1 <- spiders[,-1] # The `spiders` dataframe without the first column!

# Ok, now we'll use that trait data to run the PCA

# First, to get all of our traits on the same scale, we'll convert them to 
# z-scores. You don't have to do this, but it is fairly common, and it can be
# helpful down the line:
spiders.scale <- scale(spiders1, scale=TRUE, center=TRUE)

# Run the PCA with `princomp()`
PCAmodel <- princomp(spiders.scale, cor=FALSE)

# In princomp, the default is to use the covariance matrix. If you want to use 
# the correlation matrix, then use cor=TRUE. Here we converted to z-scores 
# first, so all variables are on the same scale and we can use the covariance 
# matrix. (The correlation matrix is the scaled covariance matrix, recall.)

summary(PCAmodel)

# This shows amount of variance explained by each axis. It's actually a big wide
# table, with all 38 principle components. It tells us the amount of variance 
# explained by each axis, and the cumulative proportion of variance explained
# Axis 1 will always explain the most variation, Axis 2 the second most, etc.

# We can plot that data easily in a scree plot
plot(PCAmodel, type="lines")

# Or even prettier:
library(factoextra)
fviz_eig(PCAmodel)

PCAmodel$loadings  # Shows the loadings of each trait on each PC 
# --- make sure you scroll up, this is a long readout. 

PCAmodel$scores # Gives output of the principal components for each individual 
# on each PC --- another 

# If we want to use the PC scores for something else (maybe we want to use 
# PC1 to run an ANOVA?), then we can pull out that vector.

PC1 <- PCAmodel$scores[,1] # Pulls out the first column of the table 
PC1

# And we could run a quick ANOVA to see if guilds differ in PC1 scores. 
# (This works because the ordering of rows in the PC scores table is the same 
# as the order in the original data table.)

model1 <- lm(PC1~spiders$Guild)
anova(model1)


# Another common thing to do with PCA is to make a biplot
biplot(PCAmodel, xlab="PC1", ylab="PC2")
# The black numbers are each individual spider. The red lines are the vectors 
# for each trait.

# There are tons of options for playing around with the biplot. I'll just do 
# this to make this look a bit better so we can see differences among guilds

# I like using ggbiplot
library(devtools)
install_github("vqv/ggbiplot") #You only need to download this package once

# In this biplot code, I'm going to separate the spiders by guild (so I need to
# use my non-subsetted data file). So I use the original file to refer to 
# guilds, but the PCA data for everything else. I'm also going to put confidence 
# intervals around the individuals in each guild.

library(ggbiplot)
ggbiplot(PCAmodel, obs.scale=1, var.scale=1, groups=spiders$Guild, ellipse=TRUE, varname.size=3, varname.adjust=1.2, circle=FALSE) +
  scale_color_discrete(name='') +
  geom_point(aes(colour=factor(spiders$Guild)), size = 1) + #Color codes by Guild
  theme(legend.direction = 'horizontal', legend.position='bottom', legend.text=element_text(size=8))

# YOU TRY IT

# The Palmer Penguins dataset is a good candidate for PCA (and originally meant
# to demonstrate PCA and related multivariate analyses. Load the package with
# this data set and use PCA as we've seen above to (1) identify which measures
# contribute to most variation among the three penguin species in the dataset, 
# then (2) visualize those differences and how they cluster in PCspace.

library("palmerpenguins") # load the data, which will be in dataframe `penguins`
glimpse(penguins)

# YOUR CODE HERE