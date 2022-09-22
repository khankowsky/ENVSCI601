## RScs0302_3rd.r
## Copied from the R user's manual by Eugene.Gallagher@umb.edu
## Last revised 9/12/21

library("Sleuth3")
library("Hmisc")
library("lattice")

attach(case0302)  
str(case0302)    # Note: Level 1 of Veteran is "Other" (first alphabeticall)

boxplot(Dioxin ~ Veteran)  

t.test(Dioxin ~ Veteran, var.equal=TRUE,
       alternative="less") # 1-sided t-test; alternative: group 1 mean is less  
t.test(Dioxin ~ Veteran, alternative="less", var.equal=TRUE, 
       subset=(Dioxin < 40)) # t-test on subset for which Dioxin < 40  
t.test(Dioxin ~ Veteran, alternative="less", var.equal=TRUE, 
       subset=(Dioxin < 20))    
t.test(Dioxin ~ Veteran, var.equal=TRUE) # 2-sided--to get confidence interval 

## HISTOGRAMS FOR PRESENTATION  
opar <- par(no.readonly=TRUE)  # Store device graphics parameter settings
par(mfrow=c(2,1), mar=c(3,3,1,1)) # 2 by 1 layout of plots; change margins    
myBreaks <- (0:46) - .5    # Make breaks for histogram bins  
hist(Dioxin[Veteran=="Other"], breaks=myBreaks, xlim=range(Dioxin),
     col="green", xlab="", ylab="", main="")     
text(10,25, 
     "Dioxin in 97 'Other' Veterans; Estimated mean =  4.19 ppt (95% CI: 3.72 to 4.65 ppt)",
     pos=4, cex=.75) # CI from 1-sample t-test & subset=(Veteran="Other")    
hist(Dioxin[Veteran=="Vietnam"],breaks=myBreaks,xlim=range(Dioxin),
     col="green", xlab="", ylab="", main="")   
text(10,160,
     "Dioxin in 646 Vietnam Veterans; Estimated mean =  4.26 ppt (95% CI: 4.06 to 4.64 ppt)",
     pos=4, cex=.75)   
text(13,145,"[Estimated Difference in Means: 0.07 ppt (95% CI: -0.63 to 0.48 ppt)]",
     pos=4, cex=.75)  
par(opar) # Restore previous graphics parameter settings

# Gallagher addendum

## Side by side lattice box plots (from Murrell p 132)
plot1<-bwplot(Dioxin ~ Veteran,xlab="Treatment",
              ylab="Dioxin",case0302) ## Vertical boxplots
plot2<-bwplot(log(Dioxin) ~ Veteran,xlab="Treatment",
              ylab="ln(Dioxin)",case0302) ## Vertical boxplots
print(plot1, position=c(0,0,1/2,1), more=TRUE)
print(plot2, position=c(1/2,0,1,1))

detach(case0302)  

## Additional Code from MSU's Robison Cox
MASS::boxcox(lm(I(Dioxin+.05) ~ Veteran, data=case0302),lam=seq(0,1,.1))
## 0.4 is the appropriate transform
## Here is the Box Cox transformation from p. 280 of Draper & Smith.
geoMean<- exp(mean(log(case0302$Dioxin)))
lambda<-0.4
% case0302$W<-((case0302$Dioxin^lambda) - 1)/(lambda*(geoMean^(lambda-1)))
case0302$W<-case0302$Dioxin^0.4
## Side by side lattice box plots (from Murrell p 132)
plot1<-bwplot(Dioxin ~ Veteran,xlab="Treatment",
              ylab="Dioxin",case0302) ## Vertical boxplots
plot2<-bwplot(W ~ Veteran,xlab="Treatment",
              ylab="Box-Cox Transform (lambda=0.4)",case0302) ## Vertical boxplots
print(plot1, position=c(0,0,1/2,1), more=TRUE)
print(plot2, position=c(1/2,0,1,1))


t.test(W ~ Veteran, var.equal=TRUE,
       alternative="less", data=case0302) # 1-sided t-test; alternative: group 1 mean is less  
# Back transformed means
c(1.729152,1.694535)^2.5 
t.test(W ~ Veteran, alternative="less", var.equal=TRUE, 
       subset=(Dioxin < 40), data=case0302) # t-test on subset for which Dioxin < 40  
c(1.724725,1.694535 )^2.5
t.test(W ~ Veteran, alternative="less", var.equal=TRUE, 
       subset=(Dioxin < 20), data=case0302)
c(1.721776,1.694535)^2.5 
t.test(W ~ Veteran, var.equal=TRUE, data=case0302) # 2-sided--to get confidence interval 
(diff= (1.729152-1.694535)^2.5)
# with 95% CI
-.11501032^2.5 
0.04577698^2.5  

# Nick Horton's mosaic analysis
summary(case0302)
favstats(Dioxin ~ Veteran, data = case0302)

# These two plots require that the user hit return at the console
bwplot(Veteran ~ Dioxin, data = case0302)
densityplot(~Dioxin, groups = Veteran, auto.key = TRUE, data = case0302)

# Two-sample t test
t.test(Dioxin ~ Veteran, var.equal = TRUE, alternative = "less", data = case0302)
# get confidence interval for the difference
t.test(Dioxin ~ Veteran, var.equal = TRUE, data = case0302)$conf.int

# We will remove two extreme observations from the data. First we remove observation 646 and
# perform a t-test (Display 3.7, page 67).
case0302.2 = case0302[-c(646), ]
t.test(Dioxin ~ Veteran, alternative = "less", data = case0302.2)

# Next we remove observations 645 and 646 and perform a t-test.
dim(case0302)

case0302.3 <- case0302[-c(645, 646), ]
dim(case0302.3)

t.test(Dioxin ~ Veteran, alternative = "less", data = case0302.3)
