# # ecotox_drc_binomial   
# drc analysis for ecotoxicology data on earthworm movement.
# dose-response package
# Program adapted by Eugene.Gallagher@umb.edu 11/3/21
# Code taken from 
# Suppement1 from 
# Ritz, C., F. Baty, J. C. Streibig, and D. Gerhard. 2015. Dose-response
# analysis using R. PLOS One 10(12): e0146021.

library(drc)
library(tidyverse)
str(earthworms)
data(earthworms)

# I think the S1 file has these reversed. In the 1st model the upper limit was 
# estimated. In the lower model, the upper limit d was set at 0.5
## Fitting an extended logistic regression model
## where the upper limit is estimated
earthworms.m1 <- drm(number/total ~ dose, weights = total, data = earthworms,
                     fct = LL.3(), type = "binomial")
summary(earthworms.m1)
##
## Model fitted: Log-logistic (ED50 as parameter) with lower limit at 0 (3 parms)
##
## Parameter estimates:
##
## Estimate Std. Error t-value p-value
## b:(Intercept) 1.505679 0.338992 4.441641 0e+00
## d:(Intercept) 0.604929 0.085800 7.050498 0e+00
## e:(Intercept) 0.292428 0.083895 3.485636 5e-04

## Fitting an extended logistic regression model
## where the upper limit is fixed at 0.5, not a good fit
earthworms.m2 <- drm(number/total ~ dose, weights = total, data = earthworms,
                     fct = LL.3(fixed = c(NA, 0.5, NA)), type = "binomial")
summary(earthworms.m2)
##
## Model fitted: Log-logistic (ED50 as parameter) with lower limit at 0 (2 parms)
##
## Parameter estimates:
##
## Estimate Std. Error t-value p-value
## b:(Intercept) 1.646689 0.376494 4.373742 0
## e:(Intercept) 0.377269 0.076785 4.913299 0
# e is the ED50
options(digits=3)
confint(earthworms.m2)

# Plot the analyses (for the EnvSci121 tox lab, it should be % germinating)
plot(earthworms.m1, broken = TRUE, type = "all",
     xlab = "Toxic substance", xlim = c(0, 100),
     ylab = "% Remaining")

# This shows the effect of fixing the upper y limit at 0.5, bad fit.
plot(earthworms.m2, broken = TRUE, type = "all",
     xlab = "Toxic substance", xlim = c(0, 100),
     ylab = "% Remaining")

# new dose levels as support for the line
newdata <- expand.grid(dose=exp(seq(log(0.005), log(10), length=100)))
# predictions and confidence intervals
pm <- predict(earthworms.m1, newdata=newdata, interval="confidence")
# new data with predictions
newdata$p <- pm[,1]
newdata$pmin <- pm[,2]
newdata$pmax <- pm[,3]
# plot curve
library(ggplot2)
# need to shift conc == 0 a bit up, otherwise there are problems with coord_trans
earthworms$dose0 <- earthworms$dose
earthworms$dose0[earthworms$dose == 0] <- 0.005
# plotting the curve
ggplot(earthworms, aes(x = dose0, y = number/total)) +
  geom_point() +
  geom_ribbon(data=newdata, aes(x=dose, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(data=newdata, aes(x=dose, y=p)) +
  coord_trans(x="log") +
  geom_vline(xintercept = 0.3773, linetype="dashed", 
              color = "blue", size=1) +
  xlab("Ethylene glycol") + ylab("% Germinating") # Labels changed for EnvSci121