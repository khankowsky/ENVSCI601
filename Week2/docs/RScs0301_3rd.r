## RScs0301_3rd.r
# Copied from the Sleuth3 R user's manual by Eugene.Gallagher@umb.edu
# Box Cox code from Jim Robison Cox's Sleuth Chapter 2 code.
## Revised 2/17/13, 2/21/13, 2/9/14, 9/12/21

library(mosaic)
library(Sleuth3)

attach(case0301) 
str(case0301) #Seeded is level 1 of Treatment (it's first alphabetically)

boxplot(Rainfall ~ Treatment) 

boxplot(log(Rainfall) ~ Treatment) # Boxplots of natural logs of Rainfall 

t.test(log(Rainfall) ~ Treatment, var.equal=TRUE,
       alternative="greater") # 1-sided t-test; alternative: level 1 mean is greater


myTest <- t.test(log(Rainfall) ~ Treatment,  var.equal=TRUE,
                 alternative="two.sided") # 2-sided alternative to get confidence interval
exp(myTest$est[1] - myTest$est[2])  # Back-transform estimate on log scale 
exp(myTest$conf) # Back transform endpoints of confidence interval 

boxplot(log(Rainfall) ~ Treatment, 
        ylab="Log of Rainfall Volume in Target Area (Acre Feet)", 
        names=c("On 26 Seeded Days", "On 26 Unseeded Days"), 
        main="Distributions of Rainfalls from Cloud Seeding Experiment") 

## POLISHED BOXPLOTS FOR PRESENTATION:
opar <- par(no.readonly=TRUE)  # Store device graphics parameters
par(mar=c(4,4,4,4))   # Change margins to allow more space on right
boxplot(log(Rainfall) ~ Treatment, ylab="Log Rainfall (Acre-Feet)",
        names=c("on 26 seeded days","on 26 unseeded days"), 
        main="Boxplots of Rainfall on Log Scale", col="green", boxlwd=2,
        medlwd=2, whisklty=1, whisklwd=2, staplewex=.2, staplelwd=2,
        outlwd=2, outpch=21, outbg="green", outcex=1.5      )        
myTicks <- c(1,5,10,100,500,1000,2000,3000) # some tick marks for original scale 
axis(4, at=log(myTicks), label=myTicks)   # Add original-scale axis on right    
mtext("Rainfall (Acre Feet)", side=4, line=2.5) # Add right-side axis label 

par(opar)  # Restore previous graphics parameter settings

# Gallagher addenda

# Clear the existing graph before running this code.
# set up two graphs side-by-side, using Zuur et al. (2009) p. 130
# A numerical vector of the form c(bottom, left, top, right) which gives the
# number of lines of margin to be specified on the four sides of the plot.
# The default is c(5, 4, 4, 2) + 0.1.
op <- par(mfrow = c(1, 2), mar=c(5,5,4,2))
boxplot(Rainfall ~ Treatment, case0301, main="On the acre-feet scale",
        ylab="Rainfall\n(acre-feet)")
boxplot(log(Rainfall) ~ Treatment, case0301, main="log scale",
        ylab="ln(Rainfall (acre-feet))")
par(ask = TRUE)
par(op)


# Use the Box-Cox Transformation to find the appropriate transform
MASS::boxcox(lm(I(Rainfall) ~ Treatment, data=case0301),lam=seq(-1,1,.1))
# Focus on the area near 0.
## I () is the R asis function. 
MASS::boxcox(lm(I(Rainfall) ~ Treatment, data=case0301),lam=seq(-.5,.5,.01))
# I() doesn't appear to be required here

MASS::boxcox(lm(Rainfall ~ Treatment, data=case0301),lam=seq(-.5,.5,.01))

case0301$logRainfall <- with(case0301, log(Rainfall))
t.test(logRainfall~Treatment, alternative='two.sided', conf.level=.95, 
       var.equal=TRUE, data=case0301)

# load the car library
## default for Levene's test is center=median, SPSS is center = mean
library(car)
OUT=leveneTest(Rainfall ~ Treatment, center=mean, data=case0301)
OUT   
OUTLN=leveneTest(log(Rainfall) ~ Treatment, center=mean, data=case0301)
OUTLN

detach(case0301) 

# Nick Horton's mosaic code
# copied by Eugene.Gallagher@umb.edu 1/15/13, revised 9/12/21

trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 3, show.signif.stars = FALSE)
summary(case0301)
favstats(Rainfall ~ Treatment, data = case0301)

bwplot(Rainfall ~ Treatment, data = case0301)

densityplot(~Rainfall, groups = Treatment, auto.key = TRUE, data = case0301)

case0301$lograin = log(case0301$Rainfall)
favstats(lograin ~ Treatment, data = case0301)
bwplot(lograin ~ Treatment, data = case0301)

densityplot(~lograin, groups = Treatment, auto.key = TRUE, data = case0301)

t.test(Rainfall ~ Treatment, var.equal = FALSE, data = case0301)

t.test(Rainfall ~ Treatment, var.equal = TRUE, data = case0301)

summary(lm(lograin ~ Treatment, data = case0301))

ttestlog = t.test(lograin ~ Treatment, data = case0301)
ttestlog

obslogdiff = -diff(mean(lograin ~ Treatment, data = case0301))
obslogdiff

multiplier = exp(obslogdiff)
multiplier
ttestlog$conf.int
exp(ttestlog$conf.int)


