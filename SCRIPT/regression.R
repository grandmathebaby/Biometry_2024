# Code for doing regression in R
# Casey terHorst, revisions by Jeremy Yoder
# 2024.09.21

# Clear the environment, load the tidyverse
rm(list=ls())

library("tidyverse")

# Import the data. 
# We'll use our old friend, the SnailData as an example
mydata <- read.csv("Data/SnailData.csv")
glimpse(mydata)

# I find NA's make it hard to deal with some regression plots, so I'm just 
# going to get rid of that NA right up front, but there are other ways of 
# dealing with NAs later (na.omit, etc) later, if you prefer
mydata <- mydata[-26,]
# In this command, I'm telling R to remove a whole line from the data
# You refer to specific data points by [row#, column#]. Bc I left the column
# number blank but specified the row number, R understands I'm referencing ALL
# columns in the row; and then the minus sign means consider all rows EXCEPT 
# the given row number(s). So x[-y, ] gives all rows in x except row y.

# We're going to ask whether the length of the snail determines its weight.
# Previously we ran this as a correlation and we could debate which is more 
# appropriate, but for illustrative purposes, we're going to run a regression.

# So let's fit a model that describes that relationship
# The command 'lm' fits a linear model. The dependent variable goes before the 
# ~ and independent variable(s) go after --- this is the usual formatting for 
# model structures across many R functions.

model1 <- lm(Weight~Length, data=mydata)

# lm = linear model
# Weight is the dependent variable (y)
# Length is the independent variable (x)

# Before we look at the results, let's test the assumptions of the model
# We can call the residuals of the model as:
resid(model1) # better to save these as something, so:
model1res <- resid(model1)

# Now we can test the normality of the residuals
library(car)
qqp(model1res, "norm") # are the residuals normally distributed?
## Yes Very

# We can also call the fitted y values as:
fitted(model1) # This is the predicted value of y for each value of x

# To test homogeneity of variance, we plot the fitted (predicted) values
# against the residuals
plot(model1res~fitted(model1))

# This looks pretty good! We want to see no pattern. If we see a cone pattern, 
# that is problematic. A cone pattern --- a narrower range of residuals at 
# smaller fitted values --- would mean that there is more variance in y for some 
# values of x than others.

# A shortcut to doing all of the above. The default plot() function applied to a
# lm object ...
plot(model1)
# gives plots showing normality, homogeneity of variance, and potential outliers
# Note that you have to go down to the R terminal and hit Return to see each 
# separate plot.

# Most of these are familiar from what we've just done. The fourth plot, 
# Residuals vs Leverage, compares residuals to a statistic that reflects how 
# much the fitted values rely on each data point --- a higher leverage value
# means that an observation is doing more to "drive" the fit of the regression
# model. High leverage *may* indicate that an outlier value should be discarded.

# We should also look at a plot of the data just to see if there are any 
# obvious non-linear patterns in the data
plot(Weight~Length, data=mydata)
abline(model1)

# or, in ggplot (my preference)
ggplot(mydata, aes(x=Length, y=Weight)) + 
  geom_point(color="lightseagreen") +
  geom_abline(intercept=coefficients(model1)[1], slope=coefficients(model1)[2]) +
  theme_bw(base_size=18)

# This looks pretty linear!

# Ok, if we're satisfied with our assumptions being met, then let's see how well 
# our model fits the data
# To see the results of the model

summary(model1)

# The information we want from this summary is the F test (on the last line)
# and the R^2 value (on the second to last line). The F test tells us whether
# the model explains more variation in the dependent variable than expected
# due to chance, and the R^2 value is the measure of total variation explained.

# We could instead run this as a Model II regression, especially if we're 
# interested in knowing the true estimate of the slope. Model II is appropriate 
# because we have error in our measures of both x and y.
library(lmodel2)
model2 <- lmodel2(Weight~Length, range.y="relative", range.x="relative", data=mydata, nperm=99)
model2
# This is a more involved readout than the summary from a standard lm. However, 
# you should be able to find familiar elements. Notably:
# - Correlation coefficient (r) and r^2
# - Permutation-based estimated p-values for whether OLS and RMA regression slopes
#   are significantly greater than zero
# - Permutation-based CIs for the intercept and slope of OLS and RMA regressions

# Note: "relative" means the variable has a true zero at some point. You can 
# also use 'interval' to indicate that the variables can include negative values
# The top of the output gives the R^2 value and p-values for a one and two-
# tailed test. Then there is a table for estimates of the slop and intercept. 
# OLS, or "ordinary least squares" should match what you found above with 
# default lm. But if you've run Model II, you want the RMA (reduced major axis)
# regression.

# how to make plot
# There are lots of options for making a plot, but I'll stick with ggplot: 
library(tidyverse)
ggplot(mydata, aes(x=Length, y=Weight))+
  geom_smooth(method = "lm", formula = y~x, color="white") + # regression line behind points
  geom_point(color="darkorange") + # points
  labs(y = "Weight (mg)", x="Length (cm)") +
  theme_bw(base_size=18)

# The shaded area in this plot is the 95% confidence interval of the 
# regression line. The geom_smooth() function with option `method="lm"` calls
# the same function we've already used, default `lm()` to estimate an OLS
# regression line with the formula given. (It will default to y~x if you don't 
# specify a formula.) The 95% CI shading is estimated using the `predict()`
# function --- it reflects error in the fitted value of y for a given x value.

# TRY IT YOURSELF
# From the Palmer Penguins data set, select two measures from one species and 
# - fit a linear model that reflects a biologically realistic relationship 
#   between the two measures
# - confirm that the data meet the assumptions of the linear regression model
# - generate a publication-quality plot of the two variables and the linear
#   regression line

# YOUR CODE HERE


