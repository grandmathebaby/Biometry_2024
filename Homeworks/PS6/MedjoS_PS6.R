#--Sacha Medjo-Akono
#--Problem Set 6 - Fall 2024
#--Sacha Medjo-Akono
#--Problem Set 5 - Fall 2024
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
library(lme4)
library(pscl)
library(factoextra)
library("pwr")
library(MASS)
library(MoMAColors)
display.all.moma()
#--Question 1
rm(list=ls())
fishy <- read_csv("Homeworks/PS6/BahamasFish.csv")
View(fishy)
#
fish <- fishy[,-1] #Remove the site column
fishzscore <- scale(fish, scale=TRUE, center=TRUE) #z-scores
PCAmodel <- princomp(fishzscore, cor=FALSE)
summary(PCAmodel) 
plot(PCAmodel, type="lines")
#--Question 2
library(factoextra)
fviz_eig(PCAmodel)
#
PCAmodel$loadings #this extracts the vectors
PCAmodel$scores
#
library(devtools)
library(ggbiplot)
ggbiplot(PCAmodel, obs.scale=1, var.scale=1, groups=fishy$Site, ellipse=TRUE, varname.size=3, varname.adjust=1.2, circle=FALSE) +
  scale_color_discrete(name='') +
  geom_point(aes(colour=factor(fishy$Site)), size = 1) + #Color codes by Guild
  theme(legend.direction = 'horizontal', legend.position='bottom', legend.text=element_text(size=8)) +
  theme_bw()
# Another common thing to do with PCA is to make a biplot
biplot(PCAmodel, xlab="PC1", ylab="PC2")
#--Question 3
PCAmodel$loadings #this extracts the vectors
PCAmodel$scores
PC1 <- PCAmodel$scores[,1] # Pulls out the first column of the table 
PC1
#
fishmod <- lm(PC1~fishy$Site)
anova(fishmod)
#--Question 4
rm(list=ls())
liz <- read_csv("Homeworks/PS6/lizardcolor.csv")
View(liz)
library("MASS")
library("tidyverse")
#Discriminant Function Analysis
liz.dfa <- lda(substrate~., data=liz, CV=FALSE) #warning ok
liz.dfa #LD1 0.6966 \ LD2 0.3034 
#
lizpred<- predict(liz.dfa)
assignmatrix <- table(liz$substrate, lizpred$class)
# R just automatically calls the groups "class" in the output
diag(prop.table(assignmatrix, 1))
# shows % of correct assignments to each group
sum(diag(prop.table(assignmatrix))) # Gives total correct assignment
#--Question 5
rm(list=ls())
rumpus <- read_csv("Homeworks/PS6/bumpus.csv")
View(rumpus)
#Male
male <- rumpus[rumpus$Sex == "m", ]
male
view(male)
#Female
female <- rumpus[rumpus$Sex == "f", ]
female #lots of NA
view(female)
#perMANOVA Male
library(vegan)
male2 <- male[, c(-1,-2, -3)]
male1 <- male[, c(-1,-2, -3, -4)]
MaleperManova <- adonis2(male1~Survival, male, permutations = 999, method="bray")
MaleperManova
#
disper <- vegdist(male1)
betadisper(disper, male$Survival) #Pretty similar avg dist to median good
#See what traits distinguish the most
MaleperManova2 <- adonis(male1~Survival, male, permutations = 999, method="bray")
MaleperManova2$coefficients
#perMANOVA female
female1 <- female[, c(-1,-2, -3, -4)]
FemaleperManova <- adonis2(female1~Survival, female, permutations = 999, method="bray")
FemaleperManova
#
disper <- vegdist(female1)
betadisper(disper, female$Survival)
#See what traits distinguish the most
FemaleperManova2 <- adonis(female1~Survival, female, permutations = 999, method="bray")
FemaleperManova2$coefficients

#nMDS plot Male
ord <- metaMDS(male1,k=2, distance='bray')
ord <- metaMDS(male1,k=2, distance='bray', trymax = 30)

ord$stress
ord2 <- metaMDS(male1,k=3, distance='bray', trymax=30)
ord2$stress

stressplot(ord2)
ordiplot(ord2) 
ordiplot(ord2, type = 'text')

plot(1, type='n', xlim=c(-0.015,0.015), ylim=c(-0.013,0.013), xlab='nMDS1', 
     ylab='nMDS2', xaxt='n', yaxt='n')

points(ord2$points[male$Survival=='TRUE',1],ord2$points[male$Survival=='TRUE',2], 
       pch=19, col='palevioletred', cex=2)
points(ord2$points[male$Survival=='FALSE',1],ord2$points[male2$Survival=='FALSE',2], 
       pch=19, col='olivedrab', cex=2)


ordiellipse(ord2, groups=male$Survival, label=F, kind='ehull', border='white', col=c('olivedrab','palevioletred'), lwd=2, draw ='polygon')
ordiellipse(ord2, groups=male$Survival, kind='sd', border='white', col=c('olivedrab','palevioletred'), lwd=2, draw ='polygon')

ordihull(ord2, groups=male$Survival, col=c('olivedrab','palevioletred'))
ordispider(ord2, groups=male$Survival, col=c('olivedrab','palevioletred'), label = T)# #POLYGON
legend('topleft', legend = paste('3D stress = ', round(ord2$stress,2)), bty='n')
legend('topright',legend=c('Survived','Died'),
       col=c('palevioletred','olivedrab'), pch=19, bty='n')

#nMDS plot Female
ordF <- metaMDS(female1,k=2, distance='bray') 
ordF <- metaMDS(female1,k=2, distance='bray', trymax = 30)

ordF$stress
ordF2 <- metaMDS(female1,k=3, distance='bray', trymax=30) 
ordF2$stress

stressplot(ordF)
ordiplot(ordF) 
ordiplot(ordF, type = 'text')
#
plot(1, type='n', xlim=c(-0.020,0.020), ylim=c(-0.013,0.013), xlab='nMDS1', 
     ylab='nMDS2', xaxt='n', yaxt='n')

points(ordF2$points[female$Survival=='TRUE',1],ordF2$points[female$Survival=='TRUE',2], 
       pch=19, col='salmon', cex=2)
points(ordF2$points[female$Survival=='FALSE',1],ordF2$points[female$Survival=='FALSE',2], 
       pch=19, col='cornflowerblue', cex=2)

ordiellipse(ordF2, groups=female$Survival, label=F, kind='ehull', border='white', col=c('cornflowerblue','salmon'), lwd=2, draw ='polygon')
ordiellipse(ordF2, groups=female$Survival, kind='sd', border='white', col=c('salmon','cornflowerblue'), lwd=2, draw ='polygon')

ordihull(ordF, groups=female$Survival, col=c('salmon','cornflowerblue'))
ordispider(ordF, groups=female$Survival, col=c('salmon','cornflowerblue'), label = T)#POLYGON
legend('topleft', legend = paste('3D stress = ', round(ordF2$stress,2)), bty='n')
legend('topright',legend=c('Survived','Died'),
       col=c('salmon','cornflowerblue'), pch=19, bty='n')