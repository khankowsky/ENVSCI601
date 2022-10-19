# RScs1001_3rd
# From Sleuth3 manual
# Polynomial regression added by Gallagher 5/26/13, 
# revised 10/16/21 with car::S, dbetas,Cook's D, minor tweaks 10/15/22

library(AICcmodavg)
library(car)   #for the S summary, residualPlots, InfluencePlot, dfbetas
library(Sleuth3)

str(case1001)
attach(case1001)  # attaching files is poor coding practice, but it works
## EXPLORATION
plot(Distance ~ Height)

## Analysis
myLm <- lm(Distance ~ Height)  
height2 <- Height^2
myLm2 <- update(myLm,  ~ . + height2)
S(myLm2, adj.r2=T)
AICc(myLm2)  # The AICc, small sample Akaike Information Criterion
height3 <- Height^3
myLm3 <- update(myLm2, ~ . + height3)
residualPlots(myLm3, smooth=T)
influencePlot(myLm3)
anova(myLm3)
S(myLm3, adj.r2=T)
AICc(myLm3)
anova(myLm2,myLm3)
height4 <- Height^4
myLm4 <- update(myLm3, ~ . + height4)
residualPlots(myLm4, smooth=T)
# The follwing plot produces an error due to ill-conditioning
# S(myLm4,adj.r2)
# AICc(myLm4)

## Section 10.2.3 (p 280-281) Predict the distance at 250 punti
# Code from Dalgaard p 197 identical to Sleuth Display 10.7 (p 281)
pred.frame<-data.frame(cbind(Height=250,height2=250^2,height3=250^3))
pred.frame
pp<-predict(myLm2,newdata=pred.frame,interval="pred", se=T)
pp
pc<-predict(myLm2,newdata=pred.frame,interval="conf", se=T)
pc

## Not in Sleuth: an alternate way of fitting the data, with Orthogonal
# polynomials see Fox & Weisberg (2019) p 190
myLm5 <- lm(Distance ~ poly(Height, 4))
S(myLm5)  # From the car package, replaces summary, Includes the AIC
AICc(myLm5)   # Note that this is inf, not enough cases
# Final Model
myLm6 <- lm(Distance ~ poly(Height, 3))
S(myLm6)  # From the car package, replaces summary, Includes the AIC
AICc(myLm6)

## analysis of dfbetas (Fox & Weisberg car p. 402-403)
dfbs.myLm2 <- dfbetas(myLm2)
plot(dfbs.myLm2[,c("Height","height2")], main="Case 8.1 dfbetas")
showLabels(dfbs.myLm2[ , "Height"],
           dfbs.myLm2[ , "height2"],
          labels=rownames(myLm2), method = "r",n=2)
#          labels=rownames(myLm2), method="identify")
#          "identify" is the default, but won't work with R Markdown'
#           remember to exit from point identification mode
# Two potential outliers identified: points 1 and 7

## DISPLAY FOR PRESENTATION 
plot(Distance ~ Height, xlab="Initial Height (Punti)",
     ylab="Horizontal Distance Traveled (Punti)",
     main="Galileo's Falling Body Experiment",
     pch=21, bg="green", lwd=2, cex=2)
dummyHeight     <- seq(min(Height),max(Height),length=100)         
betaQ           <- myLm2$coef  
quadraticCurve  <- betaQ[1] + betaQ[2]*dummyHeight + betaQ[3]*dummyHeight^2  
lines(quadraticCurve ~ dummyHeight,col="blue",lwd=3)  
betaC           <- myLm3$coef # coefficients of cubic model  
cubicCurve      <- betaC[1] + betaC[2]*dummyHeight + betaC[3]*dummyHeight^2 +
  betaC[4]*dummyHeight^3  
lines(cubicCurve ~ dummyHeight,lty=3,col="red",lwd=3) 
legend(590,390,legend=c(expression("Quadratic Fit  "*R^2*" = 99.0%"),
                        expression("Cubic Fit        "*R^2*" = 99.9%")),  
       lty=c(1,3),col=c("blue","red"), lwd=c(3,3))

detach(case1001)
