# Data visualization with ggplot2
# Jeremy Yoder
# last revised 2024.09.04
#
# This script will walk through some data visualization options to break out 
# results from multiple experimental conditions, using features of ggplot2, 
# which is one of the tidyverse packages.
# 
# Throughout, we'll work with data published by Vallejo-Marín et al, in 
#
#	Vallejo-Marín M, CE Pereira Nunes, and AL Russell. 2022. Anther cones
# 		increase pollen release in buzz-pollinated Solanum flowers. Evolution.
#		76(5): 931-945. doi.org/10.1111/evo.14485
#
# This paper tests the hypothesis that in "buzz-pollinated" flowering plant 
# species --- which release pollen when visiting bees vibrate the anthers at a 
# specific frequency --- the release of pollen is improved by modifications of 
# the anthers that link them together to vibrate as a group. Not all buzz-
# pollinated species have "functionally joined" anthers, so the authors experi-
# mentally joined the anthers of three Solanum species with unjoined anthers. 
# They measured pollen release from flowers with and without experimentally joined 
# anthers, stimulating them with vibrations from an electric shaker tuned to 
# vibrate at a specific "bee-like" frequency.
# 
# Supporting data for the paper is posted on Dryad, at doi.org/10.5061/dryad.s1rn8pk9p
# --- we'll work with the table presenting pollen output results

# SETUP
# clear the active memory
rm(list=ls())

# load the tidyverse
library(tidyverse)

# read in the pollen release data
buzz <- read.csv("Data/pollen.csv")

# INSPECT THE DATA
# what's in the `pollen` dataframe?
glimpse(buzz) # view a quick summary of the contents of each column

# from the Dryad repo readme file, here's an explanation of each column
#   Sample_ID = sample id
#   Plant_ID	= plant id
#   Flower_ID = flower id
#   Treatment = treatment (joined/free)
#   Flower_Age =flower age (days)
#   Buzz_number = buzz number
#   Sex = flower sex
#   Inflorescence = inflorescence number
#   Date = date
#   Species = species
#   Accession = plant accession
#   pollen_rem_a = pollen removed in first subsample
#   pollen_rem_b = pollen removed in second subsample
#   pollen_rem_c = pollen removed in third subsample
#   pollen_rem_d = pollen removed in fourth subsample
#   pollen_rem_est = sum of all pollen removed, multiplied by the total sample 
#     volume, according to this formula:  
#     pollen_rem_est = sum(pollen_rem_a + b + c +d) * 5
#   newflowerid = long flower id

# We'll be focusing on `pollen_rem_est`, the sum of pollen removed, and how it 
# differs by experimental treatment and species.

# We can quickly see the unique values for the different treatments, and the
# sample sizes within the treatment groups, using `table()`
table(buzz$Treatment)
table(buzz$Species)
table(buzz$Species, buzz$Treatment)

# And then let's have a look at how our focal variable is distributed
# using the ggplot2 histogram function
ggplot(buzz, aes(x=pollen_rem_est)) +
  geom_histogram() +
  annotate(x=MeanBottom, y=SDBottom, xmax=9, xmin=0, ymax=9, ymins=0 )

# `geom_histogram()` defaults to breaking the data into 30 bins, but also throws
# a warning advising you not to accept the default blindly. Take a look at how 
# bin width/number changes the information you get from the plot:

ggplot(buzz, aes(x=pollen_rem_est)) + geom_histogram(bins=10)
ggplot(buzz, aes(x=pollen_rem_est)) + geom_histogram(bins=50)

# The guidelines in our lecture suggest, for a dataset with 240 observations:

ggplot(buzz, aes(x=pollen_rem_est)) + geom_histogram(bins=9)

# Is that better than the default?
#-- They all look a little weird

# However we bin the data, they look very non-normal! We can confirm this as 
# you've already seen with functions from the `moments` package:
library(moments)
skewness(buzz$pollen_rem_est) # normal would be close to 0; this is right-skewed
kurtosis(buzz$pollen_rem_est) # normal would be less than 3; this is heavy-tailed

# and use a qq plot
library(car)

qqp(buzz$pollen_rem_est, "norm")

# what if we transform the data?
qqp(log10(buzz$pollen_rem_est), "norm")
skewness(log10(buzz$pollen_rem_est))

ggplot(buzz, aes(x=log10(pollen_rem_est))) + geom_histogram()
# or
ggplot(buzz, aes(x=pollen_rem_est)) + geom_histogram() + scale_x_log10()

# that does look somewhat better

# VISUALIZE DATA BY GROUPS
# One of the best things ggplot does is to separate groups of data in your
# visualization. There are multiple options for this. You can give each group
# different coloration, like so:
ggplot(buzz, aes(x=pollen_rem_est, color=Treatment, fill=Treatment)) + 
  geom_histogram(position="dodge") + scale_x_log10()
# here, we define `color` (the outline of bars, in geom_histogram) and `fill`
# (the color filling the bars) by values in the `Treatment` column, and ggplot
# knows to break out those groups with separate color and fill. 
#
# The `position="dodge"` option tells geom_histogram to plot bars in different 
# groups rather than on top of each other (`position="identity"`, the default).
# 
# The default theming is a little less than stellar, for this. Let's make it
# better with the cleaner `bw` theme, a custom color palette, and a less 
# awkward placement for the legend
ggplot(buzz, aes(x=pollen_rem_est, fill=Treatment)) + 
  geom_histogram(position="dodge", color="white") + 
  scale_fill_manual(values=c("#7fc97f","#fdc086"), name="Anther status") + 
  scale_x_log10() + theme_bw() + 
  labs(x="Total pollen removed", y="Observations", title="Anther joining and pollen removal") +
  theme(legend.position="bottom")

# Setting `color` within `geom_histogram` rather than linking it with a variable
# via aes() means that all bars get the same color value, a white outline. For
# the two treatment fill values, I pulled two options from a "qualitative" scale
# on the colorbrewer2.org utility.

# YOU TRY IT
# Make a new version of the previous figure, but instead of breaking out 
# observations by anther treatment (free/joined), break them out by species.
# Use colorbrewer2.org to pick a new, three-valued scale for the bar fill.

ggplot(buzz, aes(x=pollen_rem_est, fill=Species)) + 
  geom_histogram(position="dodge", color="white") + 
  scale_fill_manual(values=c("#7fc97f","#fdc086","#9bc9fa"), name="Anther status") + 
  scale_x_log10() + theme_bw() + 
  labs(x="Total pollen removed", y="Observations", title="Anther joining and pollen removal") +
  theme(legend.position="bottom")

# VISUALIZE DATA BY GROUPS IN FACETS
# When you have multiple grouping levels, you may want to handle them in 
# different ways visually. To break out the pollen release observations by both
# experimental treatment and species, we could use geom_histogram with *facets*.

ggplot(buzz, aes(x=pollen_rem_est, fill=Species)) + 
  geom_histogram(position="dodge", color="white", bins=10) + 
  scale_fill_manual(values=c("#9bc9fa","#fda083","maroon"), name="Anther status") + 
  facet_wrap("Species") +
  scale_x_log10() + theme_bw() + 
  labs(x="Total pollen removed", y="Observations", title="Anther joining and pollen removal") +
  theme(legend.position="bottom")

# Faceting breaks the data out into panels with repeated visualizations for 
# subsets of the data based on the column specified in the faceting function.
# For `facet_wrap`, a single column name (as a character string) defaults to 
# a row of facets, or multiple rows if there end up being enough facets. You 
# can also enforce a different orientation for the facets:
ggplot(buzz, aes(x=pollen_rem_est, fill=Treatment)) + 
  geom_histogram(position="dodge", color="white", bins=10) + 
  scale_fill_manual(values=c("#9bc9fa","#fda083"), name="Anther status") + 
  facet_wrap("Species", nrow=3) +
  scale_x_log10() + theme_bw() + 
  labs(x="Total pollen removed", y="Observations", title="Anther joining and pollen removal") +
  theme(legend.position="right")

# That may be better if we're interested in comparing variation along the x-
# axis, as we are in this case.

# We can also facet based on BOTH the grouping variables, using facet_grid():
ggplot(buzz, aes(x=pollen_rem_est, fill=Treatment)) + 
  geom_histogram(position="dodge", color="white", bins=10) + 
  scale_fill_manual(values=c("#7fc97f","#fdc086"), name="Anther status") + 
  facet_grid(Treatment~Species) +
  scale_x_log10() + theme_bw() + 
  labs(x="Total pollen removed", y="Observations", title="Anther joining and pollen removal") +
  theme(legend.position="right")
# The facet_grid() function takes a statement in R's formula notation, in which
# the variable (not a character string, this time) left of the ~ is the y-
# axis and the variable to the right is the x-axis, analogous to y~x in a linear
# model or regression. This faceting is maybe not super-helpful for the pollen
# data here, because we want a closer comparison of the two experimental treat-
# ments than it really allows.

# BEYOND HISTOGRAMS 1: BOXPLOTS
# For data like the pollen counts here, histograms give us some sense of spread
# but they're not our only option for comparing treatment groupings within the
# three Solanum species. 
# 
# We can do boxplots, for instance:
ggplot(buzz, aes(y=pollen_rem_est, x=Treatment, fill=Treatment)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#9bc9fa","maroon"), name="Anther status") + 
  facet_wrap("Species") +
  scale_y_log10() + theme_bw() + 
  labs(y="Total pollen removed", x="Anther status", title="Anther joining and pollen removal") +
  theme(legend.position="none")

# The geom_boxplot() function generates box and whisker plots with the median
# (central thick bar) the box extending from the 25th to 75th percentile, 
# whiskers extending 1.5x IQR (inter-quartile range) above and below the median,
# and data points beyond that 1.5x IQR plotted as outlier points.
#
# Note that we've flipped our measurement, `pollen_rem_est` to the y axis, and 
# adjusted labels accordingly. We've also used `Treatment` as a categorical x-
# axis variable, and this lets us remove the legend (legend.position="none")
# because there's now a direct visual explanation of the different groups in
# each facet, on the axis labels.

# YOU TRY IT
# Make a new version of the previous figure, but instead of faceting by species
# identity and using treatment as a grouping/x-axis variable, try faceting by
# treatment and use species as the grouping/x-axis variable. Make sure you
# change the axis labels so everything still correctly reflects the data.
# Does this new setup work better than the original? (Think about what compar-
# isons are easy to make in each of the two figures.)

ggplot(buzz, aes(y=pollen_rem_est, x=Species, fill=Species)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#9bc9fa","#756bb1","#c51b8a"), name="Anther status") + 
  facet_wrap("Treatment") +
  scale_y_log10() + theme_bw() + 
  labs(y="Total pollen removed", x="Anther status", title="Anther joining and pollen removal") +
  theme(legend.position="none")

# BEYOND HISTOGRAMS 2: BOXPLOTS WITH POINTS
# Boxplots can often be useful to summarize variation, but they're summaries,
# not direct depictions of the data. One option to address this is to simply
# plot your data WITH a boxplot.
ggplot(buzz, aes(y=pollen_rem_est, x=Treatment, fill=Treatment, color=Treatment)) + 
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  geom_jitter(width=0.25, pch=21, color="white", size=2) +
  scale_fill_manual(values=c("#7fc97f","#fdc086"), name="Anther status") + 
  scale_color_manual(values=c("#7fc97f","#fdc086"), name="Anther status") + 
  facet_wrap("Species") +
  scale_y_log10() + theme_minimal() + 
  labs(y="Total pollen removed", x="Anther status", title="Anther joining and pollen removal") +
  theme(legend.position="none")
# This gives us the summary visuals of boxplots, but we can actually see the 
# values in the data, as "jittered" points. The geom_jitter() function plots 
# points along the y axis according to their values, but adds random noise to 
# their placement on the x axis so you can see points with similar y values. 
# Some other design notes here:
# - when you set a color or fill within a geom_x() function, it will override
#   whatever is set via the aes() for the overall plot
# - `alpha` is transparency, scaled from 0 (fully transparent) to 1 (fully opaque)
# - `pch`, in point-type geoms, sets the character type used for plotting points.
#   Setting `pch=21` selects a filled circle, with fill set by `fill` and outer
#   circle color set by `color`.

# Yet another way to present more of your data while also giving a summary of
# its dispersion is a "raincloud" plot, which is possible with functions in the 
# `ggrain` package
install.packages("ggrain")
library("ggrain")
ggplot(buzz, aes(y=pollen_rem_est, x=Treatment, fill=Treatment)) + 
  geom_rain(alpha=0.5, 
            boxplot.args.pos = list(width = 0.1, position = position_nudge(x = 0.2)),
            violin.args.pos = list(side = "r",width = 0.55, position = position_nudge(x = 0.3))
            ) + 
  scale_fill_manual(values=c("#7fc97f","#fdc086"), name="Anther status") + 
  facet_wrap("Species") +
  scale_y_log10() +
  theme_minimal() + 
  labs(y="Total pollen removed", x="Anther status", title="Anther joining and pollen removal") +
  theme(legend.position="none")

# TWO VARIABLES
# Finally, we often want to know about more than variation in one measurement. 
# What are your ggplot options to display relationships between measurements?

# load a new data file, with multiple measures for snails in different sites
# and environments
snails <- read.csv("Data/SnailData.csv")
glimpse(snails)

# previously, we examined the relationship between snails' length and weight
# using the "base" plot() function
plot(Weight~Length, data=snails)

# In ggplot, this is how we'd make that same scatterplot, using `geom_point()`
# and an `aes()` statement that assigns variables to the x and y axes.
ggplot(snails, aes(x=Length, y=Weight)) + geom_point() + theme_minimal()

# GGplot also gives us a geom to illustrate regression relationships
# there are multiple options here, through geom_smooth()
ggplot(snails, aes(x=Length, y=Weight)) + 
  geom_smooth(color="white", method="lm") +
  geom_point() + 
  theme_minimal()

# The `geom_smooth()` function with `method="lm"` attempts to fit a linear
# regression to the data --- this is NOT a formal statistical test, because 
# there's no hypothesis testing. It's just drawing a line with confidence int-
# ervals, without determining whether the slope of that line is different from
# 0, as we'd see if there was no relationship between snail Length and Weight.

# All of the ggplot features to break data out by grouping variables also work
# with `geom_point()` and `geom_smooth()`

ggplot(snails, aes(x=Length, y=Weight, color=Location, fill=Location)) + 
  geom_smooth(color="white", method="lm") +
  geom_point() + 
  theme_minimal()

# YOU TRY IT
# Working with the snail data again, visualize the relationship between Length
# and Weight broken out by TidalHeight and Location, with color-coding of your
# choice.

# [Your code here]
ggplot(snails, aes(x=Length, y=Weight, color=TidalHeight, fill=TidalHeight)) + 
  geom_smooth(color="white", method="lm") +
  geom_point() + 
  facet_wrap("Location") +
  theme_minimal()