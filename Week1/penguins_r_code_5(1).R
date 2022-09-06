# penguins_r_code_5.R
# written by Garrett Evensen 2/26/21, modified by Phillip Dugger and 
# Gallagher 2/28/21:3/1/21, 3/2/2021, revised 3/11/21, 9/6/21, 9/7/21

# Install the package tidyverse using the install button on the lower right 
# panel before loading it with the library command

library(tidyverse)

# Make sure to set the working directory for where your penguins.csv file is!
# Gallagher's working directory for R course files  for Week 01 EnvSci601 is 
setwd("M:/EnvSci601/R/r_projects/Wk01_IntroR_Ch01_F21/docs")
# or click on the Session button --> Set Working Directory --> Choose Directory
?read_csv #let's see how we can load our csv file into an R dataframe

penguins <- read_csv(file = "../data/penguins.csv") #load penguins data

penguins #see data, note NA values in red, means there is no data for those points

# View the penguins dataframe
View(penguins) 
# Close the view window when done by clicking x on the top of the view

# Two other ways of analyzing the structure of the penguins dataframe:
glimpse(penguins)

str(penguins)

class(penguins)

# Use a tidyverse pipe %>% to count the number of individuals of each species.
penguins %>% #get count of each species
  count(species)

penguins %>% #Use a pipe to get mean of bill length, but 2 species have NAs. Let's fix that
  group_by(species) %>% 
  summarise(mean.bill.length = mean(bill_length_mm)) #getting mean of bill length for each species

# Create a new dataframe penguins2 that gets rid of NA ('Not Available') cases
penguins2 <- penguins %>% #make a new dataframe that gets rid of NA values
  drop_na()

View(penguins2) #great! no NAs, but 11 fewer cases 333 vs. 344
str(penguins2)

penguins2 %>% #re-calculate the means with a tidyverse pipe
  group_by(species) %>%
  summarise(mean.bill.length = mean(bill_length_mm))

# Make a scatterplot of bill depth (x axis) vs bill length (y axis) and color 
# the points by species 
ggplot(data=penguins2, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
  #brings in the data, tells ggplot which columns, colors by species
  geom_point() + #tells ggplot to plot it with points
  stat_smooth(method=lm, formula=y~x)  + #adds a least squares regression line for each species and sex
  theme_classic() + 
  labs(title="Palmer Penguins: Bill Depth vs. Bill Length", 
  x= "Bill Depth (mm)", y = "Bill Length (mm)") #sets the title, x and y axis labels

# Make a scatterplot of bill depth (x axis) vs bill length (y axis) and color 
# the points by species & plot sex with different symbols
ggplot(data=penguins2, aes(x = bill_depth_mm, y = bill_length_mm, color = species, shape=sex)) + #brings in the data, tells ggplot which columns, colors by species
  geom_point() + #tells ggplot to plot it with points
  stat_smooth(method=lm, formula=y~x)  + #adds a least squares regression line for each species and sex
  theme_classic() + labs(title="Palmer Penguins: Bill Depth vs. Bill Length", 
  x= "Bill Depth (mm)", y = "Bill Length (mm)") #sets the title, x and y axis labels

# Let's make a coplot with different graphs for each species, but the same x,y scales
# The theme is also changed back to the ggplot default, a gray scale
ggplot(data=penguins2, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
  #brings in the data, tells ggplot which columns, colors by species
  geom_point() + #tells ggplot to plot it with points
  stat_smooth(method=lm, formula=y~x)  + 
  #adds a least squares regression line for each species and sex
  facet_wrap(~species) +
  labs(title="Palmer Penguins: Bill Depth vs. Bill Length", 
       x= "Bill Depth (mm)", y = "Bill Length (mm)") #sets the title, x and y axis labels

# Let's make a coplot with different graphs for each species, but the same x,y scales, add sex
# The theme is also changed back to the ggplot default, a gray scale
ggplot(data=penguins2, aes(x = bill_depth_mm, y = bill_length_mm, color = species, shape=sex)) + #brings in the data, tells ggplot which columns, colors by species
  geom_point() + #tells ggplot to plot it with points
  stat_smooth(method=lm, formula=y~x)  + 
  #adds a least squares regression line for each species and sex
  facet_wrap(~species) +
   labs(title="Palmer Penguins: Bill Depth vs. Bill Length", 
    x= "Bill Depth (mm)", y = "Bill Length (mm)") #sets the title, x and y axis labels

# Fit a least squares regression of bill depth and bill length in Gentoos
# but first let's make a dataframe for each of the three species
adelie <- filter(penguins2, species == "Adelie") #make a dataframe for just 
# adelie, only takes rows that have Adelie in the species column
adelie
chinstrap <- filter(penguins2, species == "Chinstrap")
gentoo <- filter(penguins2, species == "Gentoo")

# Make a scatterplot of Bill Depth (mm) [x] vs. Bill Length (mm) [y] for gentoo penguins
ggplot(data=gentoo, aes(y = bill_length_mm, x = bill_depth_mm)) + # brings in the data, tells ggplot which columns, colors by species
  geom_point() + # tells ggplot to plot it with points
  stat_smooth(method=lm, formula=y~x)  + #adds a least squares regression line
  theme_classic() + labs(title="Gentoo penguins: Bill Depth vs. Bill Length", x= "Bill Depth (mm)", y = "Bill Length (mm)")
# labs sets the title, x and y axis labels

# now we will do least squares regression, lm stands for 'linear model' of 
# bill_length vs. bill depth for Gentoo penguins.

?lm #check the arguments for lm() to see what we need to add to the function
# the tilde symbol '~' means 'is a function of'
g.lm <- lm(bill_length_mm ~ bill_depth_mm, data = gentoo) 
#the formula is y ~ x, so the y variable has to go first!
g.lm
attributes(g.lm) #this let's you see what is in the g.lm object
g.lm$coefficients #get intercept and slope values

# get the p value for the regression (Ho: the slope is 0) from the final column of an ANOVA table
anova(g.lm)


# After invoking this command, move the cursor to the Hit <return> in console
# and hit return 4 times to see the residual plots for this regression.

# Make a box plot of bill_length_mm for each species
ggplot(penguins2, aes(species, bill_length_mm, color = species)) +
  geom_boxplot() + #tells ggplot to make a box plot 
  theme_classic() + labs(title="Palmer Penguins: Species vs. Bill Length",
  x="Species",y="Bill Length (mm)")

# Do a t test to compare bill lengths in Adelie and Gentoo penguins
?t.test #see the arguments for a t.test

# test whether we can reject the null hypothesis that
# adelie bill length = gentoo bill length; note the use of the $ sign to
# indicate which variable is being tested from the two different data frames
t.test(adelie$bill_length_mm, gentoo$bill_length_mm, alternative = "less") 
# Try doing the t test again, but change "less" to "greater" what happens?
t.test(adelie$bill_length_mm, gentoo$bill_length_mm, alternative = "greater")
# Now do the t test again, but change to a two-tailed or two sided alternative
# hypothesis
t.test(adelie$bill_length_mm, gentoo$bill_length_mm, alternative = "two.sided")

# Do boxplots of sexual dimorphism in mass among species:
ggplot(penguins2,aes(x=species,y=body_mass_g,fill=sex)) +
  geom_boxplot()+labs(title="Palmer Penguins", x = "Species",  
  y="Body Mass (g)")

# plot all variables vs. each other
# Install the Ggally package which has the function ggpairs and plot all
# variables pairwise
library(GGally)
# plot should include only variables: species, bill_length_mm, bill_depth_mm,
# flipper_length_mm, body_mass_g, sex
# The indices for these columns will be used to create an index vector c
# c stands for concatenate or combine
# this vector c will contain 1,3,4,5,6,7
c <- c(1,3:7)

ggpairs(penguins2,columns=c, ggplot2::aes(colour=factor(species)),
        title='Palmer Penguins')
