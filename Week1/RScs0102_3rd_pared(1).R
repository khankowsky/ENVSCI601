# RScs0102_3rd_pared.r
# Case Study 1.2 in
# Ramsey, F.L. and Schafer, D.W. (2013). The Statistical Sleuth:
#    A Course in Methods of Data Analysis (3rd ed), Cengage Learning.
# RScs0102_3rd_no2sample. 
# Plotting data but without some of the code and no 2-sample tests.
# Code modified by Eugene.Gallagher@umb.edu
# From Sleuth3 help file, Nathaniel Horton code, 
# Install and load car, Sleuth3, HMisc and lattice mosaic packages
# Modified 2/2/14, 2/5/14, 2/7/14, 2/13/14, 9/6/21, 9/7/21, 9/8/21, 9/10/21

library(CarletonStats) # for Adam Loy's tidy-Sleuth3 code
library(dplyr)      # for Adam Loy's tidy-Sleuth3 code
library(ggformula)
library(Hmisc)  # for Harrell's program for back-to-back histograms
library(lattice) # used for histograms
library(mosaic) # Horton's program with function for summarizing data
library(Sleuth3) # for Adam Loy's tidy-Sleuth3 code

# We begin by reading the data and summarizing the variables, 4 ways
data(case0102)
summary(case0102)
str(case0102)
head(case0102) # Loads the first 6 cases for viewing
# favstats from mosaic (Horton's code)
favstats(Salary ~ Sex, data = case0102)

# from Adam Loy's tidy-Sleuth3, use a tidyverse pipe to summarize cas0102
case0102 %>%
  group_by(Sex) %>%
  summarise(min = min(Salary), Q1 = quantile(Salary, probs = .25),
            median = median(Salary), Q3 = quantile(Salary, probs = .75),
            max = max(Salary), mean = mean(Salary), sd = sd(Salary))

# ggplot box, histogram, and density plot from Adam Loy 
# & the ggformula package
gf_boxplot(Salary ~ Sex, data = case0102) # display 1.12
gf_histogram(~ Salary | Sex, data = case0102, binwidth = 400) # display 1.4
gf_density(~Salary, fill = ~Sex, data = case0102)

boxplot(Salary ~ Sex, data=case0102,
        ylab= "Starting Salary (U.S. Dollars)", 
        names=c("61 Females","32 Males"),
        main= "Harris Bank Entry Level Clerical Workers, 1969-1971")

# Horton's code:
bwplot(Salary ~ Sex, data = case0102)

# densityplot from the lattice package
densityplot(~Salary, groups = Sex, auto.key = TRUE, data = case0102)

# Gallagher code with modifications by David Winsemius (1/31/12 post on R-help):
# histogram is a plotting function from the lattice package.
histogram(~ Salary | Sex, data=case0102,
               scales=list(x=list(at=seq(4000,8000,by=2000),
               labels=sprintf("$%4.0f", seq(4000,8000,by=2000)))),
                 endpoints = c(3500, 8500), layout = c(1,2), aspect = 1,
                 xlab = "Salary", main="Case 1.2")
                 

diffSex<-diff(with(case0102, tapply(Salary,Sex, mean)))
sprintf('The difference in salary was %8.4f',diffSex)

# Permutation test from Nick Horton
obsdiff = diff(mean(Salary ~ Sex, data = case0102))
obsdiff

numsim = 999
res = do(numsim) * diff(mean(Salary ~ shuffle(Sex), data = case0102))
densityplot(~Male, data = res)
confint(res)

# Add the Manly-Legendre convention for simulations to add 1 for the actual
# This prevents reporting a p-value of 0, which it never is.
p = (sum(abs(res$Male) >= abs(obsdiff))+1)/(numsim +1)
p

# From Chihara and Hesterberg's 1st ed. , Permutation tests code (p 41)
with(case0102, tapply(Salary, Sex, mean))
observed <- diff(with(case0102, tapply(Salary, Sex, mean)))
observed

## Perform permutation analysis of means
N <- 10^4 -1 # Number of times to repeat this process
result <- numeric (N) # Space to save the random differences
for (i in 1:N)
{ # sample of size 32 from 1 to 32 without replacement
  index <- sample (61+32, size = 32, replace = FALSE)
  result[i] <- mean (case0102$Salary [index]) - 
    mean (case0102$Salary [-index])
}

# scale is off, need to fix sometime
hist(result,breaks = 15, xlab = "xbar1 - xbar2",
     main = "Case 1.2: Permutation distribution for salaried")
abline(v = c(observed, -observed), col = "blue") # add line at observed mean diff.
Pvalue <- (sum(result >= observed)+1)/(N+1) #P-value
Pvalue

