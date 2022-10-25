## RScs1302_3rd.r
## transcribed from the Sleuth3 help file by Eugene.Gallagher@umb.edu
## Addition of some Nicholas Horton code too
## 2/2/13

library("Sleuth3")
library("BHH2")
library(lattice)  # for xyplot
library("MASS")
library("nlme")

str(case1302)
attach(case1302)

## EXPLORATION AND MODEL DEVELOPMENT
with(case1302, interaction.plot(Company, Treat, Score))

plot(Score ~ as.numeric(Company),cex=1.5, pch=21, 
     bg=ifelse(Treat=="Pygmalion","blue","light gray"))
myLm1   <- lm(Score ~ Company + Treat + Company:Treat) # Fit with interaction.
plot(myLm1,which=1)  # Plot residuals.
myLm2   <- update(myLm1, ~ . - Company:Treat) # Refit, without interaction.
anova(myLm2, myLm1)  # Show extra-ss-F-test p-value (for interaction effect).
if(require(car)){   # Use the car library                               
  crPlots(myLm2)
}
# Graphical ANOVA from Box, Hunter & Hunter, (2005), BHH2 package
anovaPlot(myLm1, stacked = TRUE, base = TRUE, axes = TRUE,
          faclab = TRUE, labels = T, cex = par("cex"),
          cex.lab = par("cex.lab"))
# The graphical ANOVA indicates a clear Pygmalion effect some indications 
# for C9:Pygmalion and C3:Pygmalion being interactions that matter.


## INFERENCE
myLm3 <- update(myLm2, ~ . - Company)  # Fit reduced model without Company.
anova(myLm3, myLm2)   # Test for Company effect.
summary(myLm2)   # Show estimate and p-value for Pygmalion effect.  
confint(myLm2,11)  # Show 95% CI for Pygmalion effect.

## DISPLAY FOR PRESENTATION
beta        <- myLm2$coef
partialRes  <- myLm2$res + beta[11]*ifelse(Treat=="Pygmalion",1,0) # partial res
boxplot(partialRes ~ Treat,  # Boxplots of partial residuals for each treatment
        ylab="Average Test Score, Adjusted for Company Effect (Deviation from Company Average)",
        names=c("19 Control Platoons","10 Pygmalion Treated Platoons"),
        col="green", boxlwd=2, medlwd=2,whisklty=1, whisklwd=2, staplewex=.2, 
        staplelwd=2, outlwd=2, outpch=21, outbg="green", outcex=1.5  )	

# xyplot from lattice package
xyplot(Score~Treat | Company,
       xlab="Treatment",
       ylab="Score")

# Fitting a mixed effects model, with company random using lme from nlme package
# see Venables & Ripley 1999, p 274-275 and Pinheiro & Bates
plot.design(case1302)
# From Pinheiro & Bates
interaction.plot(Treat,Company,Score,las=1)
mdl<-lme(Score~Treat, data=case1302, random = ~ 1|Company)
summary(mdl)
anova(mdl)
intervals(mdl)


detach(case1302)