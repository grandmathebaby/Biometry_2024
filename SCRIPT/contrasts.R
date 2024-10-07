# How to do a priori contrasts, or planned comparisons
# Casey terHorst, revisions by Jeremy Yoder
# 2024.10.02

# We'll use a dataset called "drugtest.csv".
# It shows the results of a drug test with four treatments to see if the drug 
# affects immune system function. Patients were either given nothing (control), 
# a placebo, a low dose of the drug, or a high dose of the drug. Then the number
# of white blood cells (WBC) per unit of blood was measured.

# Before they even begin the analysis, researchers decide they want to test two 
# specific hypotheses:
# (1) Does the drug have an effect? 
#     (Compare (control + placebo) vs (high + low dose)
# (2) Does the dosage of the drug matter? 
#     (Compare high to low dose)

# Clear the environment and load the tidyverse
rm(list=ls())
library("tidyverse")

#Import the data. 
drugtest <- read.csv("Data/drugtest.csv")
View(drugtest)

# It looks like Treatment imported as a character, which is fine if we were just
# doing an ANOVA, but for contrasts, we need to make sure it's a "factor". A 
# factor is a data format that R uses for convenience and storage efficiency,
# in which the unique values in a vector are converted to an ordered series of 
# integers but the vector is ALSO attached to an annotation (called the factor's
# "levels") that link the stored integers back to the original content of the 
# vector. Factors make sense to use if you have a vector with lots of repeated
# text entries (like treatments!) and they come into play in a number of R
# functions that need ordering --- such as axis or legend values and labels in
# ggplot. Here's a very quick demo:

fac.test <- factor(c("alpha", "bravo", "alpha", "charlie", "bravo", "charlie"))
fac.test # see what the console output looks like
levels(fac.test) # this function shows the levels of the factor

# Converting a factor to numeric values reveals the underlying stored integers
# (and also strips the levels annotation!)
as.numeric(fac.test)

# So let's convert that column into a factor
drugtest$Treatment <- as.factor(drugtest$Treatment)
drugtest$Treatment
levels(drugtest$Treatment)
# First, let's create a model and test our assumptions.
# Note that for contrasts we have to make our model with aov() instead of lm()
# These actually both run exactly the same ANOVA, but the contrasts only seem 
# to work with aov.
model1 <- aov(WBC~Treatment, data=drugtest)
plot(model1)
# Assumptions check looks ok

# Let's just peek at the results of that model
summary(model1)

# Ok, so no difference among treatments. But this doesn't exactly test the two 
# hypotheses we're interested in. Contrasts can give us more powerful tests of 
# our hypotheses, if we specifically want to know whether two particular treat-
# ments are different, rather than whether there are significant differences
# among all treatments.

# To start, we need to create two vectors of coefficients for our two contrasts.
# Note that R lists the treatment levels alphabetically. (This is the default
# in creating a factor from a character vector.)
levels(drugtest$Treatment)
# So in order, the factor levels are:
# 1-Control
# 2-High Dose
# 3-Low Dose 
# 4-Placebo

# We want coefficients for comparing 
# - Control AND Placebo vs High AND Low Dose
# - High Dose vs Low Dose
# Contrast vectors specify why factor levels should be compared. 
# Levels with positive coefficients are compared to those with 
# negative ones. Anything with a 0 is left out of the comparison.

c1 <- c(1,-1,-1,1)
# compares Control and Placebo (1 and 4) to High and Low Dose (2 and 3)

c2 <- c(0,1,-1,0) 
# Compare High Dose (2) to Low Dose (3)

# You must make sure that these are "orthogonal" --- each vector of comparisons 
# must sum to zero. 

# And now we combine these two vectors together into a matrix.
# cbind() combines two vectors of the same data class into one matrix
contrastmatrix <- cbind(c1,c2)
# Another consequence of the "orthogonal" requirement: if you multiply all the 
# numbers in a column of this matrix, the products must sum to zero also.
contrastmatrix

# Now we're going to attach this contrast matrix to the dataset
contrasts(drugtest$Treatment) <- contrastmatrix

# Now analyze the two contrasts like this:
summary(model1, split=
          list(Treatment=list("Effect of Drug"=1, "Effect of Dosage"=2))
        )

# The summary command includes the option: split. The split option provides a 
# list of factors where contrasts are stored. Within each factor, we also 
# provide a list which includes the names of the contrasts (i.e. each column) 
# stored in the contrasts matrix columns (i.e. contrastmatrix).

# We can also do Tukey post-hoc tests instead. This simply tests all possible 
# contrasts between pairs of treatments; it doesn't let us define pairs of 
# treatments a priori, or combine multiple treatments for comparison as we did 
# in the example above --- and the Tukey test adjusts p-values to account for
# doing many repeated tests on the same data.

# Option 1 is we continue to use aov:
model2 <- aov(WBC~Treatment, data=drugtest)
TukeyHSD(model2)
# or
# Option 2 is to go use lm as we have before:
model3 <- lm(WBC~Treatment, data=drugtest)

# and then call on the emmeans package:
library("emmeans")
emmeans(model3, pairwise ~ Treatment, adjust="Tukey")
# or to get letters for labeling significantly different groups in a figure:
library(agricolae)
HSD.test(model3, "Treatment", console=TRUE)

# And then here's how we might plot this result
graphdata <- as.data.frame(emmeans(model3, ~Treatment))

graphdata$tukey <- HSD.test(model3, "Treatment", console=FALSE)$groups$groups  
graphdata

ggplot(data=graphdata, aes(x=Treatment, y=emmean)) +
  geom_bar(width=0.5, stat="identity", fill='#beaed4') + 
  geom_text(aes(label=tukey), vjust=-8) + # You may need to tweak `vjust` and y limits to make this work
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), color="black", stat="identity", position=position_dodge(width=0.9), width=0.1) + 
  ylim(0,6) +
  ylab("WBC") +
  xlab("Treatment") +
  theme_bw(base_size=14)


# Try this out yourself with a measurement from the Palmer penguin data. Inspect 
# species differences for different measures, then pick one and test the hypothesis
# that the species significantly differ in that measure. Then, set up and test 
# the a priori contrast between Gentoo penguins and the other two species, using
# the contrast-testing procedure you practiced with the code above.

# YOUR CODE HERE
