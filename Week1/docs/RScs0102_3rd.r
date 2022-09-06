# RScs0102_3rd.r
# Case Study 1.2 in
# Ramsey, F.L. and Schafer, D.W. (2013). The Statistical Sleuth:
#    A Course in Methods of Data Analysis (3rd ed), Cengage Learning.
# Plotting data and doing t test
# Code modified by Eugene.Gallagher@umb.edu
# From Sleuth3 help file, Nathaniel Horton code, and Robison-Cox MSU code.
# Install and load Sleuth3, HMisc and lattice mosaic packages
# Modified 2/2/14, 2/5/14, 2/7/14, 2/13/14, 9/6/21, 9/7/21, 9/6/22

library(car)  # used for Boxplot
library(Hmisc)  # for Harrell's program for back-to-back histograms
library(lattice)
library(mosaic) # Horton's program
library(Sleuth3)

data(case0102)
# bad practice to attach/detach data
# attach(case0102)
str(case0102)
head(case0102) # Loads the first 6 cases for viewing

# favstats from mosaic (Horton's code)
favstats(Salary ~ Sex, data = case0102)

boxplot(Salary ~ Sex, data=case0102,
        ylab= "Starting Salary (U.S. Dollars)", 
        names=c("61 Females","32 Males"),
        main= "Harris Bank Entry Level Clerical Workers, 1969-1971")

# car Boxplot, which labels the outlier
Boxplot(Salary ~ Sex, data=case0102,
        ylab= "Starting Salary (U.S. Dollars)", 
        names=c("61 Females","32 Males"),
        main= "Harris Bank Entry Level Clerical Workers, 1969-1971")

# Horton's code:
bwplot(Salary ~ Sex, data = case0102)

# densityplot from the lattice package
densityplot(~Salary, groups = Sex, auto.key = TRUE, data = case0102)

## Histograms
attach(case0102)
hist(Salary[Sex=="Female"]) 
# pause here to save
dev.new()
hist(Salary[Sex=="Male"])
detach(case0102)

# Gallagher code with modifications by David Winsemius (1/31/12 post on R-help):
# histogram is a plotting function from the lattice package.
histogram(~ Salary | Sex, data=case0102,
               scales=list(x=list(at=seq(4000,8000,by=2000),
               labels=sprintf("$%4.0f", seq(4000,8000,by=2000)))),
                 endpoints = c(3500, 8500), layout = c(1,2), aspect = 1,
                 xlab = "Salary", main="Case 1.2")
                 
## I can't get the y labels to plot properly on this back-to-back plot, part of
# Frank Harrell's Hmisc package.

# out <- histbackback(split(case0102$Salary, case0102$Sex), probability=TRUE, xlim=c(-.001,.001),
options(digits=1)
out <- histbackback(split(case0102$Salary, case0102$Sex), probability=FALSE, xlim=c(-30,30),
                    main = 'Sleuth Case 1.2',ylab='Starting Salaries($)')
#! just adding color
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
options(digits=3)
# Plots somewhat like Display 1.13 on page 20 in 3rd edition

# Plot some distributions
histogram(rnorm(1000)) # Normal

histogram(rexp(1000)) # Long-tailed

histogram(runif(1000)) # Short-tailed

histogram(rchisq(1000, df = 15)) # Skewed

diffSex<-diff(with(case0102, tapply(Salary,Sex, mean)))
sprintf('The difference in salary was %8.4f',diffSex)

# Independent sample t test, equal variances assumed:
t.test(Salary~Sex, alternative='two.sided', conf.level=.95, var.equal=TRUE, 
  data=case0102)

# Permutation test from Horton
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

# Robison Cox's MSU simulation

observed <- diff(with(case0102, tapply(Salary,Sex, mean)))
## 818.0225

plot(Salary ~ Sex, data = case0102) # because code is numeric, gender is a factor

with(case0102, tapply(Salary,Sex, length))

# How many combinations with groups of size 61 & 32
choose(61+32,32)
trials = 10^4 -1
random_differences <-  numeric(trials)  ## set up storage space, or mailboxes
## use square brackets to refer to one mailbox at a time.
for(i in 1:trials) random_differences[i] <-  diff(with(case0102, tapply(sample(Salary),Sex, mean)))
hist(random_differences, breaks=15)
abline(v= c(-1,1) * observed, col="red")
l<-length( which(random_differences < -observed | random_differences> observed))/1000
## approximate p-value < 1 in 1000
Pvalue <- (l+1)/(trials+1)
Pvalue

##Chihara - Hesterberg Permutation tests code (p 41)
with(case0101, tapply(Score, Treatment, mean))
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


