# RScs1102_3rd
# From Sleuth3 manual, transcribed by Eugene.Gallagher@umb.edu 1/25/13
# revised 10/17/21, tweaked 10/15/22

library(AICcmodavg)  # for AICc
library(car) #scatterplotmatrix, S()
library(Sleuth3)
str(case1102)
attach(case1102)

## EXPLORATION
logRatio <- log(Brain/Liver)
logTime <- log(Time)
myMatrix <- cbind (logRatio, Days, Weight, Loss, Tumor, logTime, Treatment)
scatterplotMatrix(myMatrix,groups=Treatment,
                  smooth=FALSE, diagonal="histogram", col=c("green","blue"),
                  pch=c(16,17), cex=1.5)
# The figure is way too busy

myLm1 <- lm(logRatio ~ Treatment + logTime + Days + Sex + Weight + Loss + Tumor)
plot(myLm1, which=1)          
crPlots(myLm1) # Draw partial residual plots from car, using log(time)
                            

myLm2   <-  lm(logRatio ~ Treatment + factor(Time) + 
                 Days + Sex + Weight + Loss + Tumor)  # Include Time as a factor.
# Extra sum of squares F test indicates that factor(Time) explains more variation
anova(myLm1,myLm2)
  crPlots(myLm2) # Draw partial resdual plots. 
  

S(myLm2)  # Use backward elimination, results from car S()
AICc(myLm2)
myLm3 <- update(myLm2, ~ . - Days)   
S(myLm3)
AICc(myLm3)
myLm4 <- update(myLm3, ~ . - Sex)          
S(myLm4)
AICc(myLm4)
myLm5 <- update(myLm4, ~ . - Weight)
S(myLm5)
AICc(myLm5)
myLm6 <- update(myLm5, ~ . - Tumor)
S(myLm6)
AICc(myLm6)
myLm7 <- update(myLm6, ~ . - Loss)
S(myLm7)   # Final model for inference
AICc(myLm7)
# The AICc indicates the final model has the highest likelihood, after
# the penalization for the number of variables.

## INFERENCE AND INTERPRETATION
myTreatment <- factor(Treatment,levels=c("NS","BD")) # Change level ordering 
myLm7a  <- lm(logRatio ~  factor(Time) + myTreatment)
summary(myLm7a) 
beta <- myLm7a$coef
exp(beta[5])         
exp(confint(myLm7a,5))
# Interpetation: The median ratio of brain to liver tumor counts for barrier-
# disrupted rats is estimated to be 2.2 times the median ratio for control rats 
# (95% CI: 1.5 times to 3.2 times as large). 

## DISPLAY FOR PRESENTATION 
ratio <- Brain/Liver
jTime <- exp(jitter(logTime,.2)) # Back-transform a jittered version of logTime
plot(ratio ~ jTime, log="xy",
     xlab="Sacrifice Time (Hours), jittered; Log Scale",
     ylab="Effectiveness: Brain Tumor Count Relative To Liver Tumor Count; Log Scale",
     main="Blood Brain Barrier Disruption Effectiveness in 34 Rats", 
     pch= ifelse(Treatment=="BD",21,24), bg=ifelse(Treatment=="BD","green","orange"),
     lwd=2, cex=2)
dummyTime     <- c(0.5, 3, 24, 72)
controlTerm   <- beta[1] + beta[2]*(dummyTime==3) + 
  beta[3]*(dummyTime==24) + beta[4]*(dummyTime==72)
controlCurve  <- exp(controlTerm)
lines(controlCurve ~ dummyTime, lty=1,lwd=2)
BDTerm        <- controlTerm + beta[5]
BDCurve       <- exp(BDTerm)
lines(BDCurve ~ dummyTime,lty=2,lwd=2)
legend(0.5,10,c("Barrier disruption","Saline control"),pch=c(21,22),
       pt.bg=c("green","orange"),pt.lwd=c(2,2),pt.cex=c(2,2), lty=c(2,1),lwd=c(2,2))

detach(case1102)