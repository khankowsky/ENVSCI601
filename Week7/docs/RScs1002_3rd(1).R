#RScs1002_3rd
# Taken directly from the Sleuth3 manual by Eugene.Gallagher@umb.edu 1/24/13
# Revised 10/16/2021, revised 10/22/21 with moderndive parallel slopes model
# minor tweaks 10/15/22

library(car)
library(moderndive)  # for the parallel slopes model
library(Sleuth3)

str(case1002)
attach(case1002)

## EXPLORATION
plot(Energy~Mass, case1002, log="xy", xlab = "Body Mass (g) (log scale)",
     ylab = "Energy Expenditure (W) (log scale)", 
     pch = ifelse(Type=="echolocating bats", 19,
                  ifelse(Type=="non-echolocating birds", 21, 24)))
legend(7, 50, pch=c(24, 21, 19),
       c("Non-echolocating bats", "Non-echolocating birds","Echolocating bats"))

logEnergy  <- log(Energy)
logMass <- log(Mass)
myLm2 <- lm(logEnergy ~ logMass + Type + logMass:Type)
plot(myLm2, which=1)                
myLm3 <- update(myLm2, ~ . - logMass:Type)  
anova(myLm3, myLm2)   # Test for interaction effects with extra ss F-test

# No need for the interaction term (p 0.53, Extra sum of squares F test)

## INFERENCE AND INTERPRETATION
myLm4 <- update(myLm3, ~ . - Type)  # Reduced model...with no effect of Type
anova(myLm4, myLm3)   # Test for Type effect
# Little evidence for a type effect (p=0.66, Extra sum of squares F test)
# Change the reference category to test non-echolocating vs echolocating bats
# and non-echolocating bats vs. non-echolocating birds.
myType <- factor(Type,
                 levels=c("non-echolocating bats","echolocating bats","non-echolocating birds"))
myLm3a <- lm(logEnergy ~ logMass + myType) 
summary(myLm3a)
100*(exp(myLm3a$coef[3]) - 1) 
100*(exp(confint(myLm3a,3))-1)  
# Conclusion: Adjusted for body mass, the median energy expenditure for 
# echo-locating bats exceeds that for non-echolocating bats by an estimated 
# 8.2% (95% confidence interval: 29.6% LESS to 66.3% MORE)

## Non-integer indicator variables, not in Sleuth, see Draper & Smith for
# other examples

BirdvBat<-c(rep(-1/2,4),rep(1,12),rep(-1/2,4))
BatvBat<-c(rep(-1/2,4),rep(0,12),rep(1/2,4))
nebirdsvnebats<-c(rep(0,4),rep(-1/2,12),rep(1/2,4))
echobatsvsnonecho<-c(rep(1,4),rep(-1/2,12),rep(-1/2,4)) #echo bats - nonecholocators
case1002$BirdvBat<-BirdvBat
case1002$BatvBat<-BatvBat
myLm5 <- lm(logEnergy ~ logMass + BirdvBat + BatvBat)
S(myLm5)
myLm6<-lm(logEnergy ~ logMass + nebirdsvnebats+echobatsvsnonecho)
S(myLm6)

# DISPLAYS FOR PRESENTATION 
myPlotCode    <- ifelse(Type=="non-echolocating birds",24,21)        
myPointColor  <- ifelse(Type=="echolocating bats","green","white")  
plot(Energy ~ Mass, log="xy", xlab="Body Mass (g); Log Scale ",
     ylab="In-Flight Energy Expenditure (W); Log Scale",
     main="In-Flight Energy Expenditure Study",
     pch=myPlotCode,bg=myPointColor,lwd=2, cex=1.5) 
dummyMass  <- seq(5,800,length=50)
beta       <- myLm3$coef
curve1     <- exp(beta[1] + beta[2]*log(dummyMass))
curve2     <- exp(beta[1] + beta[2]*log(dummyMass) + beta[3])
curve3     <- exp(beta[1] + beta[2]*log(dummyMass) + beta[4])
lines(curve1 ~ dummyMass)
lines(curve2 ~ dummyMass, lty=2)
lines(curve3 ~ dummyMass, lty=3)
legend(100,3,
       c("Echolocating Bats","Non-Echolocating Bats","Non-Echolocating Birds"),
       pch=c(21,21,24),lwd=2,pt.cex=c(1.5,1.5,1.5),pt.lwd=c(2,2,2),
       pt.bg=c("green","white","white"),lty=c(1,2,3))

# plot the parallel slopes model with moderndive:geom_parallel_slopes()
ggplot(data=case1002, aes(x = log(Energy), y = log(Mass), color = Type)) +
        #brings in the data, tells ggplot which columns, colors by species
        geom_point() + #tells ggplot to plot it with points
        geom_parallel_slopes(se = FALSE) + 
        theme_classic() + 
        labs(title="Case Study 10.2", 
             x= "log(Mass)", y = "log(Energy)") #sets the title, x and y axis labels

# Now plot with standard errors
ggplot(data=case1002, aes(x = log(Energy), y = log(Mass), color = Type)) +
  #brings in the data, tells ggplot which columns, colors by species
  geom_point() + #tells ggplot to plot it with points
  geom_parallel_slopes(se = TRUE) + 
  theme_classic() + 
  labs(title="Case Study 10.2", 
       x= "log(Mass)", y = "log(Energy)") #sets the title, x and y axis labels

detach(case1002)