# How to do nMDS and perMANOVA
# Casey terHorst, revisions by Jeremy Yoder
# 2024.11.20

# Clears the environment and load the tidyverse
rm(list=ls()) 
library("tidyverse")

####
# How to do NMDS
# We're going to use some of Nyssa Silbiger's data. Nyssa went to several 
# different sites and quantified percent cover of different species, including 
# bare rock and sand.

PercentTotal <- read.csv('Data/PercentTotal.csv')
glimpse(PercentTotal)

# We're going to need to vegan library, which is an ecology-focused package 
# with lots of utilities for multivariate analyses
library("vegan")

# There are a few ways to do MDS in R. I'm going to use metaMDS, which does 
# NMDS, and the code is pretty simple.

# First, create the ordination output using a Bray-Curtis dissimilarity matrix
ord <- metaMDS(PercentTotal[,-1],k=2, distance='bray')

# We add [,-1] to get rid of the first column, which is the site names
# distance = is the type of dissimilarity matrix
# k = number of dimensions to keep (you can try different ones to look at 
# different stress values)

# If it does not converge, add more iterations by setting `trymax` higher than
# the default value (not actually necessary in this case)
ord <- metaMDS(PercentTotal[,-1],k=2, distance='bray', trymax = 30)

# Let's look at the stress with k=2 dimensions. Is it < 0.3? 
ord$stress

# It is 0.2 which is "good/ok". Would it help to add a third dimension?
ord2 <- metaMDS(PercentTotal[,-1],k=3, distance='bray', trymax=30) 
ord2$stress

# That's a little better, so we might want that one. But for simplicity,I'm 
# going to stick with two dimensions, which was still "ok".

# Let's look at the stress plot
stressplot(ord)
# We want to minimize scatter around the line, and this is a good fit

# Basic plot
ordiplot(ord) 
# dots represent sites (tide pools, in Nyssa's case) and 
# + represents species
# add text with species labels
ordiplot(ord, type = 'text')

# let's make a better plot
plot(1, type='n', xlim=c(-2,2), ylim=c(-1.5,1.5), xlab='nMDS1', 
     ylab='nMDS2', xaxt='n', yaxt='n')
# You will need to play with the x and y lim to get a graph that best shows 
# differences --- add the points and see how this works ...
points(ord$points[PercentTotal$Site=='Bodega',1],ord$points[PercentTotal$Site=='Bodega',2], 
       pch=19, col='red', cex=1)
points(ord$points[PercentTotal$Site=='BobCreek',1],ord$points[PercentTotal$Site=='BobCreek',2], 
       pch=19, col='purple', cex=1)
points(ord$points[PercentTotal$Site=='CDM',1],ord$points[PercentTotal$Site=='CDM',2], 
       pch=19, col='magenta', cex=1)
points(ord$points[PercentTotal$Site=='Monterey',1],ord$points[PercentTotal$Site=='Monterey',2], 
       pch=19, col='lightblue', cex=1)

# Let's add a circle around all points by groups
ordiellipse(ord, groups=PercentTotal$Site, label=F, kind='ehull', border='white', col=c('purple','red','magenta','lightblue'), lwd=2, draw ='polygon')
# If you want to make the circles Standard deviations
ordiellipse(ord, groups=PercentTotal$Site, kind='sd', border='white', col=c('purple','red','magenta','lightblue'), lwd=2, draw ='polygon')

# You can add or remove labels 
# Other options ... just draw a ploygon
ordihull(ord, groups=PercentTotal$Site, col=c('purple','red','magenta','lightblue'))
# add site labels
ordispider(ord, groups=PercentTotal$Site, col=c('purple','red','magenta','lightblue'), label = T)# make a spider plot
# add a legend with stress
legend('topleft', legend = paste('2D stress = ', round(ord$stress,2)), bty='n')
#add a Site legend
legend('topright',legend=c('Bob Creek','Bodega','Monterey','Corona del Mar'),
       col=c('purple','red','lightblue','magenta'), pch=19, bty='n')

# This may be an excessively busy plot, so you probably only want to add SOME 
# of the options above

####
# We could formally test whether sites are different using a perMANOVA. 
# This requires the adonis2 function in the vegan package

permanovamodel <- adonis2(PercentTotal[,-1]~Site, PercentTotal, 
                          permutations = 999, method="bray")
permanovamodel

# If we are to trust the results of the permanova, then we have to assume that 
# the dispersion among data is the same in each group. We can test with 
# assumption with a PermDisp test:
disper <- vegdist(PercentTotal[,-1])
betadisper(disper, PercentTotal$Site)

# Look at the "average distance to median" readout. These numbers should be 
# similar-ish. A general rule is that no one number should not be twice as large
# as any other.

# An option for doing post-hoc pairwise comparisons in R
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
pairwise.adonis(PercentTotal[-1], PercentTotal$Site, perm=999)

# We need coefficients to see which species contribute most to differences among 
# sampling sites, but ... adonis2() has not implemented coefficients. So for now
# we'll refit with the old, deprecated function. Hopefully this is fixed in a 
# future release of vegan. (Open source software is fun!)
permanova2 <- adonis(PercentTotal[,-1]~Site, PercentTotal, 
                     permutations = 999, method="bray")
permanova2$coefficients
# Higher absolute value means that variable drives differences among groups

