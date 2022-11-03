# Quetelet.r
# Analysis of Gallagher, E. D. 2020. Was Quetelet???s average man normal? 
# American Statistician 74: 301-306.
# Written by Eugene.Gallagher@umb.edu, 10/30/22

library(car)       # for qqPlot
library(DescTools) # for pearson test
library(fBasics) # for the ksnormTest
library(ggpubr)  # for ggdensity and ggqqplot
library(MASS)    # for fitdistr
library(nortest)
library(rstatix) # for shapiro_test
library(stats)
library(tibble) # for the tibble function
library(tidyverse) # for ggplot graphics

# Load Quetelet's (1846) data (Larsen & Marx 2018 p 504; Stigler 1986, p. 207)
ChestCircumf<-33:48
Observed<-c(3, 18, 81, 185, 420, 749, 1073, 1079, 934, 658, 370, 92, 50, 21, 4, 1)
DATA=rep(ChestCircumf,Observed)

# Fit the Quetelet chest distribution's 4 moments: mean, variance, skewness & Kurtosis
fitdistr(DATA, "normal") # from the MASS package
(VarQuetelet = var(DATA))
(SkewQuetelet<-skewness(DATA))
(KurtQuetelet<-timeDate::kurtosis(DATA,method="moment"))

# Visually display Quetelet's data for normality plot of Quetelet data 
# with normal curve, Gallagher (2020) Fig 1
Quetelet<-tibble(Chest_girth=DATA)  # need a tibble or data frame for ggplot
# overlay histogram and density plot from
# https://scientificallysound.org/2018/06/07/test-normal-distribution-r/
p1 = ggplot(Quetelet, aes(x=Chest_girth)) +
  geom_histogram(aes(y = ..density..), binwidth=1, colour="black", fill="white") +
  labs(title="Quetelet's Scottish Chest Girths", 
       x= "Chest circumference (in.)",y = "Density") +
  stat_function(fun = dnorm, lwd = 2, col = 'red', 
                args = list(mean = mean(Quetelet$Chest_girth), 
                           sd = sd(Quetelet$Chest_girth)))
p1

# Load the Stigler-corrected Quetelet data. 
# Stigler (1986, p 208) documents that Quetelet made errors in transcribing the
# data from the 1817 Edinburgh Medical and Surgical Journal. Stigler provides
# the correct data in Table 5.6 (p. 208). There were 5732, not 5738 cases and,
# of most importance in fitting the chi-square distribution, there were 168, 
# not 92, 44" chests.

observed=c(3, 19, 81, 189, 409, 753, 1062, 1082, 935, 646, 313, 168, 50, 18, 3, 1)
data<-rep(ChestCircumf,observed)

# Fit the Stigler-corrected chest data's 4 moments: mean, variance, skewness & kurtosis
fitdistr(data,"normal") # From the MASS package
(VarStigler = var(data))
(SkewStigler<-skewness(data)) # 3 is expected
(KurtStigler<-timeDate::kurtosis(DATA,method="moment")) # zero is expected

# R plot of Stigler-corrected data with normal curve, Gallagher (2020) Fig 2
# Need to create a tibble for ggplot
Stigler<-tibble(Chest_girth=data)
p4 = ggplot(Stigler, aes(x=Chest_girth)) +
            geom_histogram(aes(y = ..density..), binwidth=1, colour="black", fill="white") +
              labs(title="Stigler Corrected Scottish Chest Girths", 
            x= "Chest circumference (in.)", y = "Density") +
            stat_function(fun = dnorm, lwd = 2, col = 'red', 
                          args = list(mean = mean(Stigler$Chest_girth), 
                                      sd = sd(Stigler$Chest_girth)))
p4

#### 3 QQ plots of Stigler data

qqPlot(data, main='Stigler-corrected Scottish Chest Girths')
# QQ plot from https://www.datanovia.com/en/lessons/normality-test-in-r
ggqqplot(data, title='Stigler-corrected Scottish Chest Girths',
         xlab = "Chest circumference (in.)", ylab = "Probability",
         ggtheme=theme_classic())

z.data<-(data-mean(data))/sd(data) ## standardized data
qqnorm(z.data, main='Stigler-corrected Scottish Chest Girths') ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line

qqnorm(data, main='Stigler-corrected Scottish Chest Girths')
qqline(data)

# Empirical cumulative distribution plot of Stigler-corrected Scottish Chest Girths

# ecdf plots of Stigler Scottish Chest data
plot(ecdf(data),main="Empirical cumulative distribution function of Stigler Scottish Chests")

# Three plots of pseudorandom normal data}
# Apply rnorm function, convert Stigler mean and var to cm
datar <- rnorm(length(data), mean(data), sd(data))
Stigler_r<-tibble(Chest_girth=datar)
p5 = ggplot(Stigler_r, aes(x=Chest_girth)) +
  geom_histogram(aes(y = ..density..), binwidth=0.2, colour="black", fill="white") +
  labs(title="Pseudo-random normal Scottish Chest Girths", 
       x= "Chest circumference (in.)", y = "Density") +
  stat_function(fun = dnorm, lwd = 2, col = 'red', 
                args = list(mean = mean(Stigler_r$Chest_girth), 
                            sd = sd(Stigler_r$Chest_girth)))
p5
# Apply rnorm function, round normal data to nearest inch
datarin <- round(rnorm(length(data), mean(data), sd(data)),0)
Stigler_rin<-tibble(Chest_girth=datarin)
p6 = ggplot(Stigler_rin, aes(x=Chest_girth)) +
  geom_histogram(aes(y = ..density..), binwidth=1, colour="black", fill="white") +
  labs(title="Pseudo-random normal Scottish Chest Girths", 
       x= "Chest circumference (in.)", y = "Density") +
  stat_function(fun = dnorm, lwd = 2, col = 'red', 
                args = list(mean = mean(Stigler_rin$Chest_girth), 
                            sd = sd(Stigler_rin$Chest_girth)))
p6
# Now plot an R version of Gallagher (2020) Figure 5
datarcm <- round(rnorm(length(data), mean(data)*2.54, sd(data)*2.54),0)
Stigler_rcm<-tibble(Chest_girth=datarcm)
p7 = ggplot(Stigler_rcm, aes(x=Chest_girth)) +
  geom_histogram(aes(y = ..density..), binwidth=1, colour="black", fill="white") +
  labs(title="Pseudo-random normal Scottish Chest Girths", 
       x= "Chest circumference (cm)", y = "Density") +
  stat_function(fun = dnorm, lwd = 2, col = 'red', 
                args = list(mean = mean(Stigler_rcm$Chest_girth), 
                            sd = sd(Stigler_rcm$Chest_girth)))
p7
p8 = ggplot(Stigler_rmm, aes(x=Chest_girth)) +
  geom_histogram(aes(y = ..density..), binwidth=1, colour="black", fill="white") +
  labs(title="Pseudo-random normal Scottish Chest Girths", 
       x= "Chest circumference (mm)", y = "Density") +
  stat_function(fun = dnorm, lwd = 2, col = 'red', 
                args = list(mean = mean(Stigler_rmm$Chest_girth), 
                            sd = sd(Stigler_rmm$Chest_girth)))
p8

## Fit the normality tests to compare to Gallagher (2020) Table 4
# Anderson-Darling tests from the nortest package
AD<-adTest(DATA) # Quetelet data
AD
ad<-adTest(data) # Stigler data
ad
AD_r<-adTest(datar) # Random data with Stigler mean and sd
AD_r
ad_rin<-adTest(datarin)  # Random data with Stigler mean and sd, rounded to inches
ad_rin
ad_rcm<-adTest(datarcm) # Random data with Stigler mean and sd, rounded to cm
ad_rcm
ad_rmm<-adTest(datarmm) # Random data with Stigler mean and sd, rounded to mm
ad_rmm

# Pearson's Chi-square normality test: Not working properly yet in R, 
# Matlab is MUCH better
# PearsonTest from DescTools
PChi<-PearsonTest(DATA,adjust=FALSE)
PChi
pchi<-PearsonTest(data,adjust=FALSE)
pchi
pchi<-PearsonTest(datar,adjust=FALSE)
pchi
pchi<-PearsonTest(datarin,adjust=FALSE)
pchi
pchi<-PearsonTest(datarcm,adjust=FALSE)
pchi
pchi<-PearsonTest(datarmm,adjust=FALSE)
pchi
# It's very strange that this has such a low p value

# Neither of these tests from DescTools are correct.

# Analyze with the nortest Pearson's chi-square test
pearson.test(DATA)
pearson.test(data)
pearson.test(datar)
pearson.test(datarin)
pearson.test(datarcm)
pearson.test(datarmm)

# Cramer-von Mises normality test
# Analyze with nortest cvm.test
CVM<-cvm.test(DATA)
CVM
cvm<-cvm.test(data)
cvm
# Test statistic is correct 10.6 and 10.4, p values < 1e-9, not 0.003 like 
# Gallagher (2020)
cvm_r<-cvm.test(datar)
cvm_r
cvm_rin<-cvm.test(datarin)
cvm_rin
cvm_rcm<-cvm.test(datarcm)
cvm_rcm
cvm_rmm<-cvm.test(datarmm)
cvm_rmm

# fit the D'Agostino Pearson test
DP<-dagoTest(DATA)
DP
dp<-dagoTest(data)
dp
# Statisics and p values match Gallagher(2020) Table 4
dp_r<-dagoTest(datar)
dp_r
dp_rin<-dagoTest(datarin)
dp_rin
dp_rcm<-dagoTest(datarcm)
dp_rcm
dp_rmm<-dagoTest(datarmm)
dp_rmm

# fit the Jarque Bera Test
jarqueberaTest(DATA)
jarqueberaTest(data)
# There ia a typo in Gallagher (2020) for the Jarque-Bera test. The test
# statistics match, but Gallagher's p value should be 0.38 (p=0.3754) not 0.24
JB <-jarqueberaTest(DATA)
JB
jb<-jarqueberaTest(data)
jb
jb_r<-jarqueberaTest(datar)
jb_r
jb_rin<-jarqueberaTest(datarin)
jb_rin
jb_rcm<-jarqueberaTest(datarcm)
jb_rcm
jb_rmm<-jarqueberaTest(datarmm)
jb_rmm

# fit the Kolmogorov--Smirnov normality test
## fit with the fBasics package ksnormTest
KS<-ksnormTest(DATA)
KS
ks<-ksnormTest(data)
ks
# ksnormTest produces a warning that ks.test should not be used with ties. 
# The low p values match
ks_r<-ksnormTest(datar)
ks_r
ks_rin<-ksnormTest(datarin)
ks_rin
ks_rcm<-ksnormTest(datarcm)
ks_rcm
ks_rmm<-ksnormTest(datarmm)
ks_rmm
# Maybe ks test is affected by ties. p = eps

# A different ks test from the base stats package
ks.test(DATA, 'pnorm')
ks.test(data, 'pnorm')
ks.test(datar, 'pnorm')
ks.test(datarin, 'pnorm')
ks.test(datarcm, 'pnorm')
ks.test(datarmm, 'pnorm')

# Lilliefors (Kolmogorov-Smirnov) test from the 'nortest' package
L<-lillieTest(DATA)
L
l<-lillieTest(data)
l
# R produced 2.2e-16 for p, Matlab stops at 0.001
l_r<-lillieTest(datar)
l_r
l_rin<-lillieTest(datarin)
l_rin
l_rcm<-lillieTest(datarcm)
l_rcm
l_rmm<-lillieTest(datarmm)
l_rmm
# The Lilliefor test has p 0.1

# Fit the Shapiro-Wilk statistic (upper limit of 5000)
## fit with the base stats shapiro.test
shapiro.test(sample(DATA,5000))
shapiro.test(sample(data,5000))
# These results match, Gallagher (2020) erroneously used 0 for the p value
# but R uses 2.2e-16, or eps.
shapiro.test(sample(datar,5000))
shapiro.test(sample(datarin,5000))
shapiro.test(sample(datarcm,5000))
shapiro.test(sample(datarmm,5000))
# p = 0.23

## fit with the nortest sfTest
sfTest(sample(DATA,5000))
sfTest(sample(data,5000))
# These results match, Gallagher (2020) erroneously used 0 for the p value
# but R uses 2.2e-16, or eps.
sfTest(sample(datar,5000))
sfTest(sample(datarin,5000))
sfTest(sample(datarcm,5000))
sfTest(sample(datarmm,5000))
#  p = 0.29