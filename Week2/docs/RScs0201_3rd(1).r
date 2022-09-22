# RScs0201_3rd.r
# from Sleuth3 manual, Gallagher's & Horton's code.
# Modified by Eugene.Gallagher@umb.edu 1/13, revised 2/3/13, 2/5/14, 9/12/21,
# 9/15/21

library(aplpack) # for back-to-back stem & leaf plots
library(car)    # for Boxplot which labels outliers
library(dplyr)     # data manipulation
library(ggformula) # graphics
require(Hmisc)  # Frank Harrell's miscellaneous programs: back-to-back hist
library(Sleuth3) # Ramsey & Schafer's Statistical Sleuth 3d edition

# Take a look at the data
View(case0201) # Close after opening in the code window.
data(case0201)   
str(case0201)
head(case0201)
summary(case0201)

# Display 2.1, Sleuth3 page 30 in the text using package aplpack, which 
# apparently requires the data to be attached
attach(case0201)
stem.leaf.backback(Depth[Year == 1976],
                   Depth[Year == 1978],
                   back.to.back = TRUE,unit=0.1,
                   trim.outliers = FALSE,
                   show.no.depths = TRUE)
detach(case0201)

# Use Loy's tidyverse code to summarize the data
case0201 %>%
  group_by(Year) %>%
  summarise(min = min(Depth), Q1 = quantile(Depth, probs = .25),
            median = median(Depth), Q3 = quantile(Depth, probs = .75),
            max = max(Depth), mean = mean(Depth), sd = sd(Depth))

# Plot back-to-back histograms from Frank Harrell's Hmisc package
options(digits=1)
out <- histbackback(split(case0201$Depth, case0201$Year), probability=FALSE, 
                    xlim=c(-30,30),ylab="Beak Depth (mm)",
                    main = 'Sleuth Case 2.1')          
#! just adding color
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
options(digits=5) # return to the R default


## t tests
mean(case0201$Depth[case0201$Year==1978]) - mean(case0201$Depth[case0201$Year==1976])  
yearFactor <- factor(case0201$Year) # Convert the numerical variable Year into a factor
# with 2 levels. 1976 is "group 1" (it comes first alphanumerically)
t.test(Depth ~ yearFactor, var.equal=TRUE, data=case0201) # 2-sample t-test; 2-sided by default 
t.test(Depth ~ yearFactor, var.equal=TRUE, 
       alternative = "less", data=case0201) # 1-sided; alternative: group 1 mean is less 

## BOXPLOTS FOR PRESENTATION from Nick Horton (Smith/Amherst)
boxplot(Depth ~ Year, data=case0201,     
        ylab="Beak Depth (mm)", names=c("89 Finches in 1976","89 Finches in 1978"),  
        main="Beak Depths of Darwin Finches in 1976 and 1978", col="green", 
        boxlwd=2)
# car Boxplot which labels the outliers
Boxplot(Depth ~ Year,             
        ylab="Beak Depth (mm)", names=c("89 Finches in 1976","89 Finches in 1978"),  
        main="Beak Depths of Darwin Finches in 1976 and 1978", col="green", 
        boxlwd=2, medlwd=2, whisklty=1, whisklwd=2, staplewex=.2, staplelwd=2,  
        outlwd=2, outpch=21, outbg="green", outcex=1.5, data=case0201)
# I don't know why the outlier numbers are printed to the console.

# Use Adam Loy's tidy-Sleuth3 code for a boxplot, histogram & density plot
# A boxplot from ggplot2 through the ggformula package
gf_boxplot(Depth ~ factor(Year), data = case0201) + 
  xlab("Year") +
  coord_flip()
# Now, a histogram
gf_histogram(~Depth, data = case0201, bins = 15) +
  facet_wrap(~Year)
# A density plot
gf_density(~Depth, fill= ~factor(Year), data = case0201) +
  scale_fill_brewer("Year", palette = "Set2")


