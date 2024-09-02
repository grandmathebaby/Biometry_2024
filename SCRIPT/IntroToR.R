#Basic Stats in R
#Casey terHorst, revised by JBY
#Edited August 23, 2023
#We're going to start out working with a data file called "SnailData.csv"
#This data set has snail lengths and weights from the high and low intertidal, 
#at two locations: San Diego and Monterey

#Note the way that this data is set up. It's not normally the way we'd enter data in Excel
#perhaps, but it is the way you should format data for most stats programs. You
#can always change around the format of the data in R, but I generally find this simpler to 
#do in Excel first.

#Note that you should save your Excel file as a .csv file before importing to R

#But before we do anything, we need to set up a project.
#In the upper right corner of the R Studio window, click on the drop down list
#and select "New Project". Name the new project something like "Biometry2020"
#or whatever you want and save it in a place of your choosing.
#Now go to the lower right window of RStudio and click on "New Folder"
#Create three new folders here, named (1) Data, (2) Scripts, and (3) Output.

#Go to Canvas and under Modules, find the Data Sets module, in which there is a
#file named SnailData.csv. Download that file and move it into the Data folder
#that you just created in the previous step. If you want, you can also save this
#Script, along with any modifications you make to it, in the Scripts folder.

#Ok, now we're ready tp start using R! There are four windows in RStudio:
#
###(1) This window. This is the Script window.#### 
#You can write anything you want here.
#Writing things here is just words. They don't mean anything until you do something
#with them. We can write commands here, but they won't be executed until we send
#them to R. You can "send" a command to R to be executed by clicking CTRL+Enter (on a PC)
#or Command+Enter on a Mac. Doing so will send the current line of script to R.
#You can also highlight multiple lines of code and send these simultaneously.
#Alternatively, you can click Run at the top right of this window and it will do
#the same thing.
#
###(2) The R window###
#This is the lower left window. You can see your commands executed here.
#You can also see the results of your commands here.
#This window is really "R". If you opened R on its own, you would just have that
#single window and you would have to type or paste all your commands there.
#This program, with its four windows, is RStudio, which just makes it easier to interface
#with R. But the R window is really where all the magic happens.
#
###(3) The environment window###
#This is the upper right window. Here you can see what is available for use by R.
#You'll find any datasets you have loaded here, and you can click on them
#to see their properties and make sure they're as you'd hoped. You'll also find
#any R objects here. We'll get to how you create objects in a bit.
#
###(4) The Output/Packages window###
#This is the bottom right window. You'll find the folders you created here.
#Any plots or graphs you make will appear here. You'll also be able to load packages
#here. More info on packages in a bit.


#Clear the environment. This removes everything in the environment window.
#It's useful to always do this first, just to make sure there's nothing
#there that you don't want to interact with accidentally.
rm(list=ls())


#Load the data. The command below will work for any file that you put in the Data
#folder we created earlier. You just need to change the code so that it has the correct
#name of the file after Data/
mydata <- read.csv("Data/SnailData.csv")
#Now you've told R to import that data file and call is "mydata". You can call it
#whatever you want though! You just need to make sure you always refer to it by the name 
#you used.

#Take a look at the data object in the panel on the right. Click the arrow next to it
#This lets you see some properties of the data. Some are characters (chr). Some are numbers (num).
#Some are integers (int), which are also numbers, but they are discontinuous numbers.
#Location and TidalHeight are both characters. That's what we want. Length and Weight
#are both numbers...that's what we want too. 
#But make sure that the other columns that describe what that data point is, are factors.

#Ok, we've got our data imported, so let's look at it:
mydata #This will bring up the data in the R window below

#That's not super easy to look at in the R window.
#If you want to view it in it's own tab:
View(mydata)



#Or if we wanted to just look at (or refer to) one of those columns, say Length
mydata$Length

#or Weight
mydata$Weight

#This means "call this dataset", but the $ means, "just this column"

#Some basic things to know.
#We could just give R a command to do something:
1+5+7+3
#Or we could name it as an object
#For example, we could do this, and then we don't need to type in all that info in order
#to call it up again
Object1<- 1 + 5 + 7 + 3

#If you're doing anything remotely complicated, it makes sense to name it as an object,
#so you can call it again easy later.
Object1

#If we wanted the mean snail length
mean(mydata$Length)

#Or better, name it as an object
meanLength<-mean(mydata$Length)

#If you want to see that value, then:
meanLength

#Let's do the same for weight
meanWeight<-mean(mydata$Weight)
meanWeight

#Uh oh, run into a problem? We got an NA, which probably means there's an NA in our dataset
#This is really common. Usually R puts NA in any blank cells. It's common to have missing
#measurements for some of your cells.
#But we can get rid of those, pretty easily by naming Length and Weight as new objects
#but we'll remove the NAs in the process

meanWeight<-mean(mydata$Weight, na.rm=TRUE)
#This means, create a new object called "meanWeight" (or whatever you want to call it)
#That object will be defined as the mean of the data in this particular column,
#but please ignore the NAs. Now we call that object: 
meanWeight

#To get the standard deviation of Length
sdLength<-sd(mydata$Length, na.rm=TRUE) #gives the standard deviation
sdLength

#To get the standard error of Length. Unfortunately, there's not a pre-programmed
#function for standard error, so we have to tell R how to calculate it. We'll get to 
#this in Lecture 2, but se = sd/sqrt(n).
seLength<-sd(na.omit(mydata$Length))/sqrt(length(na.omit(mydata$Length)))
seLength #gives the standard error


#Ok, so we have some descriptive stats, but it's for all of the data. 
#You probably want the mean length of shells from each location. 

#Let's try a function called describeBy. BaseR (what you downloaded from the web)
#comes with a bunch of pre-loaded commands, but MANY people have created additional
#commands in their own packages (also called libraries). So before we can use those
#commands, we need to load that particular package. But before you can load the package, 
#you need to download it and install it. So let's download and install the "psych" package.
#You need to be connected to the internet to download a package, but once you have it, you
#don't need to be online to load it
install.packages("psych") #you only need to do this once. From now on you can skip this part
#You'll see some stuff happen in the R window as the package downloads

library(psych) 
#This loads the psych package. Note that you need to load this library every time you restart your R session
#Ok, now we can use the describeBy command

#To get a summary of all of your data, broken down by location
describeBy(mydata, group=mydata$Location)
#This says to describe mydata, but break down that description by different Locations

#We could also break this down by Tidal Height instead:
describeBy(mydata, group=mydata$TidalHeight)

#This gives you mean, sd, median, min, max, se, etc for every column in mydata.
#You probably are most interested in the results for Length and Weight though

#or to break this down by both groups:
describeBy(mydata, list(mydata$Location, mydata$TidalHeight))
#Note that we used the command "list" to tell R, ok, I'm going to give you a list
#of more than one thing.
#Side note, if you're giving a list of numbers, you have to use c, as in c(1,2,3,4,5)

#So, that's one way to get our descriptive stats.
#But now all of the cool kids are using tidyr, a.k.a the TidyVerse.
#We'll use tidyr to get the same descriptive stats, but you will find tidyr useful
#for doing MANY more things later in R. So it's useful to learn the syntax.

#We'll need one additional package: the tidyverse package of packages
install.packages("tidyverse")

#Ok, we've downloaded and installed them. Now let's load both libraries
library(tidyverse)

#There are five main commands we can use:
#1. filter(): Pick certain rows based on conditions about their values
#Useful if you want to subset your data

#2. summarize(): Compute summary measures known as “summary statistics” of variables

#3. group_by(): Group rows of observations together

#4. mutate(): Create a new variable in the data frame by mutating existing ones

#5. arrange(): Arrange/sort the rows based on one or more variables

#In tidyr language, we often use the operator %>% which you can translate as "then"
#It allows you to go from one step to the next.
#For example, let's say we just want to take a subset of just the San Diego data:

SDonly <- mydata %>%
  filter(Location == "SanDiego")
#This says to make a group I'm calling "SDonly" that takes the dataset mydata
#and then filters through it to only take rows that meet the condition that the Location
#is San Diego. The "==" means that Location must be SanDiego. You could also
#use "!=" to mean does not equal SanDiego to get all the data that is anything other 
#than SanDiego. You could also use commands like <, > if you're filtering based on numbers.
#Use | for "or" and & for "and"


SDonly

#You can also do this for multiple groups: 
#So if you wanted to subset just the Low intertidal data from San Diego:
SDlow <- mydata %>%
  filter(Location =="SanDiego", TidalHeight=="Low")

SDlow

#Ok, fine, so let's use this to summarize some descriptive stats for our data
#If we just wanted to summarize all the data
Summary_Snails<- mydata %>%
  summarize(mean=mean(Length, na.rm=TRUE), std_dev=sd(Length, na.rm=TRUE))

Summary_Snails

#You can also ask for
#min and max
#IQR (interquartile range)
#sum
#n (count the number of rows)

#Ok, but let's separate out our Locations and Tidal Heights and get means and
#standard deviations for each
SummaryByGroup <- mydata %>%
  group_by(Location, TidalHeight) %>%
  summarize(mean=mean(Length, na.rm=TRUE), std_dev=sd(Length, na.rm=TRUE))
SummaryByGroup
#Now we have means and standard deviations for all of our four groups

#This generates a table of output, called a dataframe. Note that you can always look in the Environment
#window (upper right) to see what all of your objects and dataframes are.
#You might want to refer to just a single number in that dataframe:
MontereyHigh<-SummaryByGroup[1,3] #this means you want the output in row1, column3
MontereyHigh

MontereyHighCI<-SummaryByGroup[1,4]*1.96 #gives the value in row 1, column 4 and multiplies is by 1.96
MontereyHighCI

Monly <- mydata %>%
  filter(Location == "Monterey")
#Let's use standard errors instead of standard deviations. We'll do the same thing as
#before, except with a more complex calculation for standard error
SummaryByGroup <- mydata %>%
  group_by(Location, TidalHeight) %>%
  summarize(mean=mean(Length, na.rm=TRUE), std_err=sd(Length, na.rm=TRUE)/sqrt(length(na.omit(Length))))
SummaryByGroup


#Ok, the last thing we're going to do in this Intro is to make some simple plots
#so we can visualize our data.

#Let's make some simple plots. If we want to just plot Weight vs Length
plot(Weight~Length, data=mydata)

#Note that this plot appears in the Output window. You can go there and export that
#graph to an image file or pdf, if you want. Or you can save it to your Outputs folder
#that you created.

#There are many more options to be explored in the plot function
#You can also use the command barplot to make one of these. There are tons of options
#for using these basic plot and barplot functions. It's up to you whether you want to do that.
#This is called baseplot and many people use this to make great looking graphs.

#You should decide whether you want to make all your plots in baseplot, or use ggplot.
#I like to use ggplot, not because it is better, but just because I learned it first.
#Either is fine, but I'm going to give you an example in ggplot. One is not better than 
#the other, they're just each have different syntax.
#I will supply another script that has more information on using "plot" if you prefer that instead


#Here I'm going to use ggplot to create a simple bar plot with error bars
#here's some basic code that you can modify to make a bar graph

#First we need to create a new dataframe that has our means and standard errors

#Let's first make a simple plot.
#Let's say we were only concerned with Location and we ignored Tidal Height.
#We'd make a bar plot with only two bars.
#First let's gather the means and standard errors we want to use
graphdata <- mydata %>%
  group_by(Location) %>%
  summarize(meanLength=mean(Length, na.rm=TRUE), se=sd(Length, na.rm=TRUE)/sqrt(length(na.omit(Length))))
graphdata

#Ok, now let's use the ggplot2 library, which installs with the tidyverse

#Let's make a plot now.
#ggplot works in layers. Every line of code is another layer that lays on top of the 
#graph and changes some particular component.
#Here with each new layer I'm saying, take the previous plot and add this new layer
#Then at the end, tell R to make (print) the plot for me
p<-ggplot(graphdata, aes(x=Location, y=meanLength)) #defines what x and y variables to use to make the graph
p<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))
#The above command gets rid of the gray background and the gridlines. Blech, who wants those?
p<-p + geom_bar(stat="identity", color="burlywood", fill="maroon", position="dodge", size=0.6) #what colors should the bars be?
p<-p + labs(x="Location", y="Length") #what are the labels for the axes?
p<-p + geom_errorbar(aes(ymax=meanLength+se, ymin=meanLength-se), position=position_dodge(0.9), width=0.1) #add error bars

print(p)


#Let's make another plot, but not plotting all four of our possible groups.
#We will need to create a new dataframe with the new means and se.
graphdata2 <- mydata %>%
  group_by(Location, TidalHeight) %>%
  summarize(meanLength=mean(Length, na.rm=TRUE), se=sd(Length, na.rm=TRUE)/sqrt(length(na.omit(Length))))
graphdata2

#Now we'll use ggplot again, but we have an additional grouping factor (TidalHeight).
#Note here that I am using ggplot slightly different because I am creating this all as
#one command, rather than multiple commands...but it's still the same thing, with
#each line adding one more layer to the graph
ggplot(graphdata2, aes(x=Location, y=meanLength, fill=factor(TidalHeight), group=factor(TidalHeight))) + #basic plot with TidalHeight as a grouping factor
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=meanLength+se, ymin=meanLength-se), stat="identity", position=position_dodge(width=0.9), width=0.1) + #adds error bars
  labs(x="Location", y="Snail Length", fill="TidalHeight") + #labels the x and y axes
  scale_fill_manual(values=c("Low"="maroon","High"="burlywood")) #fill colors for the bars

#There are tons and tons of colors available in R. Google the R color palette for a complete set,
#or check out https://colorbrewer2.org for curated sets of colors for different applications





