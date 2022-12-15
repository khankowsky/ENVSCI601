# SicilyCVE
# Based on Frank Harrell's example code from RMS shortcourse May 2021
require(rms)
# Uses the rms package gTrans function documented at hbio-stat.org/R/rms/gtrans.html

options (prType ='latex') # applies to printingmodelfits
getHdata (sicily ) #fetch dataset from hbiostat.org/data
d<-sicily
dd<-datadist (d); options (datadist = 'dd')
g <- function (x) exp(x) * 100000
off <-list ( stdpop = mean (d$ stdpop )) # offset for prediction (383464.4)
w <- geom_point (aes(x=time , y= rate ), data =d)
v <- geom_vline (aes( xintercept =37 , col=I('red')))
yl <- ylab ('Acute Coronary Cases Per 100,000')
f <- Glm(aces ~ offset(log(stdpop)) + rcs(time,6), data=d, family = poisson)
f$aic
# Plot the data
ggplot(Predict(f, fun=g, offset =off)) + w + v + yl

# Save knot locations
k <- attr (rcs(d$time, 6), 'parms')
k

kn <- k

# rcspline.eval is the rcs workhorse

h <-  function (x) cbind(rcspline.eval (x, kn),
                         sin=sin (2*pi*x/12), cos=cos (2*pi*x/12))
f <- Glm( aces ~ offset (log( stdpop )) + gTrans (time, h),
         data =d, family = poisson )
f$aic

ggplot (Predict(f, fun=g, offset =off)) + w + v + yl

# Next add more notes near intervention to allow for sudden change

kn <- sort (c(k, c(36 , 37, 38)))
f <-Glm( aces ~ offset (log(stdpop)) + gTrans (time , h),
         data =d, family = poisson )
f$aic

ggplot(Predict (f, fun=g, offset =off)) + w + v + yl

# Now make the slow trend simpler (6 knots) and more finely control times at
# which predictions are requested, to handle discontinuity.
# Harrell uses greater than or equal 37, but I don't know how to code that
h <- function (x) cbind ( rcspline.eval (x, k),
                         sin=sin (2*pi*x/12) , cos=cos (2*pi*x/12),
                         jump =x > 36.999)
f <-  Glm( aces ~ offset (log( stdpop )) + gTrans (time, h),
         data =d, family = poisson)
f$aic

times <- sort (c(seq (0, 60, length =200), 36.998 , 37, 37.001 ))
ggplot(Predict(f, time =times ,fun=g, offset =off)) + w + v + yl

# Look at fit statistics, especially evidence for the jump
# summary(f)
# summary generates Latex code which is unreadable, i'll regenrate f with glm
f2 <-  glm( aces ~ offset (log( stdpop )) + gTrans (time, h),
           data =d, family = poisson)
# These two lines not in Harrell's code, but match his output.
summary(f2)
anova(f2,test="Chisq")

# Strong evidence for a jump effect
# gTrans(time, h)jump    -0.126814   0.031270   -4.055 5.00e-05 ***
exp(-0.126814) # 0.8808975
# There was a decrease of 12% in cardiovascular events resulting from the
# cigarette ban
# Strong evidence for seasonality



