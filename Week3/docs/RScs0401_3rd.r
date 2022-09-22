## RScs0401_3rd.r
# Case Study 4.1 in
# Ramsey, F.L. and Schafer, D.W. (2013). The Statistical Sleuth:
#    A Course in Methods of Data Analysis (3rd ed), Cengage Learning.
# Random permutations of the data and Fisher's exact test
# Code modified by Eugene.Gallagher@umb.edu, revised 9/20/21
## Transcribed from the Sleuth3 help file, Nicholas Horton's scripts and
## Jim Robison Cox's scripts.

library("Sleuth3")
str(case0401)
attach(case0401)

(mCool <- mean(Incidents[Launch=="Cool"])) 
(mWarm <- mean(Incidents[Launch=="Warm"]))
mDiff <- mCool - mWarm
c(mCool,mWarm,mDiff)  # Show the values of these variables
(vCool <- var(Incidents[Launch=="Cool"])) 
(vWarm <- var(Incidents[Launch=="Warm"]))
c(vCool,vWarm)

## PERMUTATION TEST , VIA REPEATED RANDOM RE-GROUPING (ADVANCED)
numRep  <- 100000 # Number of random  groupings. CHANGE TO LARGER NUMBER; eg 100,000.   
rDiff   <- rep(0,numRep) # Initialize this variable to contain numRep 0s.
for (rep in 1:numRep) {  # Repeat the following commands numRep times:
  randomGroup <- rep("rWarm",24)  # Set randomGroup to have 24 values "rWarm"
  randomGroup[sample(1:24,4)]  <- "rCool"  # Replace 4 at random with "rCool"
  mW  <- mean(Incidents[randomGroup=="rWarm"]) # average of random "rWarm" group
  mC  <- mean(Incidents[randomGroup=="rCool"]) # average of random "rCool" group
  rDiff[rep] <- mC-mW  # Store difference in averages in 'rep' cell of rDiff
}  # End of loop
hist(rDiff,  # Histogram of difference in averages from numRep random groupings
     main="Approximate Permutation Distribution",
     xlab="Possible Values of Difference in Averages",
     ylab="Frequency of Occurrence")
abline(v=mDiff)  # Draw a vertical line at the actually observed difference
pValue <- sum(rDiff >= 1.3)/numRep  # 1-sided p-value
pValue  
text(mDiff,75000, paste(" -->",round(pValue,4)), adj=-0.1) 

# Horton addendum
install.packages("mosaic") # note the quotation marks
require(mosaic)
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 3, show.signif.stars = FALSE)

favstats(Incidents ~ Launch, data = case0401)

xhistogram(~Incidents | Launch, data = case0401)
t.test(Incidents ~ Launch, var.equal = TRUE, data = case0401)
# How many combinations (24 choose 4)
(C244 = factorial(24)/(factorial(4) * factorial(24 - 4)))

case0401$Incidents[c(1, 2, 4, 24)]
with(case0401, t.test(Incidents[c(1, 2, 4, 24)], Incidents[-c(1, 2, 4, 24)], var.equal = TRUE))

case0401$Incidents[c(1, 4, 5, 24)]

with(case0401, t.test(Incidents[c(1, 4, 5, 24)], Incidents[-c(1, 4, 5, 24)], var.equal = TRUE))

(C1113 = factorial(5)/(factorial(3) * factorial(5 - 3)) * 1)
(C1123 = factorial(5)/(factorial(2) * factorial(5 - 2)) * 1 * 1)
(C0123 = 17 * 5 * 1 * 1)
(C0123 = 17 * 5 * 1 * 1)

(onep = (C1113 + C1123 + C0123)/C244)

result <- t.test(Incidents ~ Launch, var.equal = TRUE, data = case0401)$statistic
result

# Random permutations, 1e5 permutations about 40 secs.
nulldist = do(100000) * t.test(Incidents ~ shuffle(Launch), var.equal = TRUE,
                                 data = case0401)$statistic
xhistogram(~t, groups = t >= result, v = result, data = nulldist)
tally(~t >= result, format = "proportion", data = nulldist)

# Jim Robison Cox's r scripts
install.packages("exactRankTests", dep=TRUE)
require(exactRankTests)
## The exactRanktests package is no longer supported by R, but it is fast
perm.test(Incidents ~ Launch,case0401, alt="greater")
# calculate the number of permutations (see text)
choose(5,2)
(choose(17,1) * choose(5,1) + choose(5,3) + choose(5,2))/ choose(24,4)

??## As described in Gallagher's 9/21/21 lecture, the Fisher test is the most appropriate
## test for these data.
LaunchData<-matrix(c(4, 0, 3, 17), nrow = 2,
       dimnames = list(Warm = c("Incident", "No Incident"),
                       Cold = c("Incident", "No Incident")))
LaunchData
fisher.test(LaunchData)
fisher.test(LaunchData,alternative="greater")
## Note that the 1-tailed and two-tailed p values are the same for this
## 2 x 2 contingency table. There is no extreme on the other tail as 
## extreme as 100% incidents on the launch. That also prevents odds ratios
## from being calculated.
AltH<-matrix(c(0, 4, 7, 13),
       nrow = 2,
       dimnames = list(Warm = c("Incident", "No Incident"),
                       Cold = c("Incident", "No Incident")))
fisher.test(AltH)
fisher.test(LaunchData,alternative="greater")

detach(case0401)