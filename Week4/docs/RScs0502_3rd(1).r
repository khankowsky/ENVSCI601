# RScs0502_3rd.r
# Code copied from the help file by
# Eugene.Gallagher@umb.edu, 1/16/2013, revised 4/8/13, 2/18/14
# 3/3/15, 9/28/21

library(BHH2)
library(mosaic)  # from Horton's mosaic project
library(Sleuth3)

str(case0502)  
attach(case0502)  # This is bad programming practice. I'll work to get rid of it

## First look at the structure of the data
str(case0502)
favstats(Percent ~ Judge, data = case0502)  # from Horton's mosaic


## Make new factor level names (with sample sizes) and analyze boxplots
myNames <- c("A (5)", "B (6)", "C (9)", "D (2)", "E (6)", "F (9)", "Spock's (9)")

boxplot(Percent ~ Judge, ylab = "Percent of Women on Judges' Venires",
        names = myNames, xlab = "Judge (and number of venires)",
        main = "Percent Women on Venires of 7 Massachusetts Judges")

myAov1  <- aov(Percent ~ Judge)
plot(myAov1, which=1)   # Residual plot
summary(myAov1) # Initial screening. Any evidence of judge differences? (yes)
summary.lm(myAov1) # A linear model, with the 1st level JudgeA being the 
                   # reference level

## ANALYSIS 1. TWO-SAMPLE t-TEST (ASSUMING NON-SPOCK JUDGES HAVE A COMMON MEAN)
SpockOrOther <- factor(ifelse(Judge=="Spock's","Spock","Other"))                                   
aovFull      <- aov(Percent ~ Judge) 
aovReduced   <- aov(Percent ~ SpockOrOther) 
anova(aovReduced,aovFull) #Any evidence that 7 means fit better than the 2 means?       
t.test(Percent ~ SpockOrOther, var.equal=TRUE) # Evidence that 2 means differ?
# Graphical ANOVA from Box, Hunter & Hunter, 2nd edition.
anovaPlot(aovFull, stacked = TRUE, base = TRUE, axes = TRUE,
          faclab = TRUE, labels = TRUE, cex = par("cex"),
          cex.lab = par("cex.lab"))

## ANALYSIS 2. COMPARE SPOCK MEAN TO AVERAGE OF OTHER MEANS 
myAov3        <- aov(Percent ~ Judge - 1)

myContrast    <- rbind(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6, - 1))
if(require(multcomp)){  # use multcomp library
  myComparison  <- glht(myAov3, linfct=myContrast) 
  summary(myComparison, test=adjusted("none"))   
  confint(myComparison) 
}

## BOXPLOTS FOR PRESENTATION   
boxplot(Percent ~ Judge,  ylab= "Percent of Women on Judges' Venires",
        names=myNames, xlab="Judge (and number of venires)",
        main= "Percent Women on Venires of 7 Massachusetts Judges",
        col="green", boxlwd=2,  medlwd=2,  whisklty=1,  whisklwd=2,
        staplewex=.2, staplelwd=2,  outlwd=2,  outpch=21, outbg="green",
        outcex=1.5)         

detach(case0502)