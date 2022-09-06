# RScs0101_3rd.r
# Ramsey & Schafer Case Study 1.1
# Code written by Eugene.Gallagher@umb.edu
# From help file and MSU code.
# Modified 1/6/13, 2/13/14, 9/5/21, 9/6/21, 9/7/21
# The code is working, but the aplpack routine required attach/detach

# Use Session tab above to set working directory to this file location

# Install and load packages
# These packages should be installed.
library(aplpack)
library(Sleuth3)
library(mosaic)  # Pruim, Kaplan & Horton's (Smith College) package for teaching statistics
# Options recommended by Horton for mosaic
options(digits = 3)
trellis.par.set(theme = col.mosaic())

# This statement is not necessary, but it will load the data in your local environment
data(case0101, package="Sleuth3")

# Analyze the structure of the data table
head(case0101) # Show the 1st 6 rows of data
str(case0101)
summary(case0101)
# if this were a tibble, not a table, one could use
# view(case0101)

# attach(case0101)  # attach/detach is now regarded as bad R programming
# favstats from Horton's mosaic package
favstats(Score ~ Treatment, data=case0101)

# from the Sleuth3 help file, with slight modifications: data = case0101
boxplot(Score ~ Treatment,data=case0101,  # Boxplots with labels
        ylab= "Average Creativity Score From 11 Judges (on a 40-point scale)",  
        names=c("23 'Extrinsic' Group Students","24 'Intrinsic' Group Students"), 
        main= "Haiku Creativity Scores for 47 Creative Writing Students")

# Individual stem-and-leaf plots, from Nicholas Horton code
with(subset(case0101, Treatment == "Extrinsic"), stem(Score, scale = 5))

with(subset(case0101, Treatment == "Intrinsic"), stem(Score, scale = 5))
# This Horton code produces and error, don't know why:
# mosaic::maggregate(Score ~ Treatment, data = case0101, FUN = stem)

# Display 1.10, page 17 in the text using package aplpack, needed attach/detach
attach(case0101)
aplpack::stem.leaf.backback(Score[case0101$Treatment == "Extrinsic"],
                     Score[case0101$Treatment == "Intrinsic"],
                     back.to.back = TRUE,unit=0.1,
                     show.no.depths = TRUE)
detach(case0101)

# Gallagher code with modifications by David Winsemius (1/31/12 post on R-help):
# histogram is a plotting function from the lattice package.
histogram(~Score | Treatment, data=case0101,
          scales=list(x=list(at=seq(4,32,by=4),
                             labels=sprintf("%2.0f", seq(4,32,by=4)))),
          endpoints = c(3.5, 32.5), layout = c(1,2), aspect = 1,
          xlab = "Creativity Scores")

# Equal variance t test
t.test(Score~Treatment, alternative='two.sided', conf.level=.95,
  var.equal=TRUE, data=case0101)

# Unequal variance or Welch t test
t.test(Score ~ Treatment, alternative = "two.sided", data=case0101)

# analyze with a linear model from N Horton

summary(lm(Score ~ Treatment, data = case0101))

# Monte Carlo simulations from N Horton's Chapter 1 pdf on his web site
diffmeans = diff(mean(Score ~ Treatment, data = case0101))
diffmeans # observed difference

numsim = 4999 # set to a sufficient number of Monte Carlo trials
nulldist = do(numsim) * diff(mean(Score ~ shuffle(Treatment), data = case0101))
confint(nulldist)

histogram(~Intrinsic, nint = 50, data = nulldist, v = c(-4.14, 4.14))

# Monte Carlo simulations: permutations from Robison-Cox
## Montana State University Sleuth Code (Jim Robison-Cox, Stat 410/511)
with(case0101, tapply(Score, Treatment, mean))
diff(with(case0101, tapply(Score, Treatment, mean)))
## randomly resample these numbers once, shuffling the codes:
diff(with(case0101, tapply(Score, sample(Treatment), mean)))

## how many ways can these be shuffled? 1.61238e+13
choose(47,23)      ## or choose(47, 24)
 
trials=499
random_differences <-  numeric(trials)  ## set up storage space, or mailboxes
  ## use square brackets to refer to one mailbox at a time.
for(i in 1:trials) random_differences[i] <-  diff(with(case0101, tapply(Score, sample(Treatment), mean)))

hist(random_differences,breaks=15)
# plot a vertical line at the observed difference of  4.144203
abline(v= c(4.14, -4.14),col="red")

 random_differences[which(random_differences < -4.14 | random_differences>4.14)]
  ## approximate (due to not seeing all 1.6e+13 possibilities) p-value
  ##  how extreme is the difference we observed relative to all
  ##  possible differences?
# Following Manly & Legendre & Legendre, add 1 for the observed difference to simulation

(length( which(random_differences < -4.14 | random_differences>4.14))+1)/(trials+1)

##Chihara - Hesterberg Permutation tests code (p 41)
with(case0101, tapply(Score, Treatment, mean))
observed <- diff(with(case0101, tapply(Score, Treatment, mean)))
observed

## Perform permutation analysis of means
N <- 10^3 -1 # Number of times to repeat this process
result <- numeric (N) # Space to save the random differences
# use a for loop to do (N-1=999) random permutations
for (i in 1:N)
  { # sample of size 24 from 1 to 47 without replacement
  index <- sample (47, size = 24, replace = FALSE)
  result[i] <- mean (case0101$Score [index]) - 
  mean (case0101$Score [-index])
}

hist(result,breaks = 15, xlab = "xbar1 - xbar2",
     main = "Case 1.1: Permutation distribution for creativity scores")
abline(v = c(observed, -observed), col = "blue") # add line at observed mean diff.

Pvalue <- (sum(result >= observed)+1)/(N+1) #P-value
Pvalue




