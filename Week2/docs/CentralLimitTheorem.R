# Program written by Nicole Radziwill at r-bloggers
# https://www.r-bloggers.com/sampling-distributions-and-central-limit-theorem-in-r/
# modified by Eugene.Gallagher@umb.edu, 10/10/2019 added QQplot

sdm.sim <- function(n,src.dist=NULL,param1=NULL,param2=NULL) {
  r <- 10000  # Number of replications/samples - DO NOT ADJUST
  # This produces a matrix of observations with  
  # n columns and r rows. Each row is one sample:
  my.samples <- switch(src.dist,
                       "E" = matrix(rexp(n*r,param1),r),
                       "N" = matrix(rnorm(n*r,param1,param2),r),
                       "U" = matrix(runif(n*r,param1,param2),r),
                       "P" = matrix(rpois(n*r,param1),r),
                       "C" = matrix(rcauchy(n*r,param1,param2),r),
                       "B" = matrix(rbinom(n*r,param1,param2),r),
                       "G" = matrix(rgamma(n*r,param1,param2),r),
                       "X" = matrix(rchisq(n*r,param1),r),
                       "T" = matrix(rt(n*r,param1),r))
  all.sample.sums <- apply(my.samples,1,sum)
  all.sample.means <- apply(my.samples,1,mean)   
  all.sample.vars <- apply(my.samples,1,var) 
  op <- par(no.readonly = TRUE)  # save plotting parameters in op
  par(mfrow=c(2,2))  # will plot figures in a 2 x 2 array
  
  hist(my.samples[1,],col="gray",main="Distribution of One Sample")
  hist(all.sample.sums,col="gray",main="Sampling Distribution of
	the Sum")
  hist(all.sample.means,col="gray",main="Sampling Distribution of the Mean")
  # hist(all.sample.vars,col="gray",main="Sampling Distribution of
	# the Variance")
  # add a qq plot for the 4th graph
  qqnorm(all.sample.means,col="gray",main="Quantile-Quantile Plot of Means")
  qqline(all.sample.means, col="red")
#  Returns the plotting parameters to their original values.
  par(op) }

# uncomment just one of these calls to the function sdm.sim at a time.
# sdm.sim(10,src.dist="B",param1=10,param2=0.5)
#  sdm.sim(50,src.dist="B",param1=10,param2=0.5)
# sdm.sim(100,src.dist="B",param1=10,param2=0.5)
# sdm.sim(10,src.dist="C",param1=0,param2=1)  # This produces results where the CLT fails
## See: https://stats.stackexchange.com/questions/74268/cauchy-distribution-and-central-limit-theorem
##  https://web.ma.utexas.edu/users/mks/M358KInstr/Cauchy.pdf
# sdm.sim(50,src.dist="C",param1=0,param2=1)
# sdm.sim(100,src.dist="C",param1=0,param2=1)
# sdm.sim(10,src.dist="E",param1=1)
# sdm.sim(50,src.dist="E",param1=1)
# sdm.sim(100,src.dist="E",param1=1)
# sdm.sim(10,src.dist="G",param1=10,param2=2)
# sdm.sim(50,src.dist="G",param1=5,param2=2)
# sdm.sim(100,src.dist="G",param1=5,param2=2)
# sdm.sim(10,src.dist="N",param1=20,param2=3)
# sdm.sim(50,src.dist="N",param1=20,param2=3)
# sdm.sim(100,src.dist="N",param1=20,param2=3)
# sdm.sim(10,src.dist="P",param1=0.5)
# sdm.sim(50,src.dist="P",param1=0.5)
# sdm.sim(100,src.dist="P",param1=0.5)
# sdm.sim(10,src.dist="T",param1=5)
# sdm.sim(50,src.dist="T",param1=5)
# sdm.sim(100,src.dist="T",param1=5)
# sdm.sim(10,src.dist="U",param1=0,param2=1) # specify min & max
sdm.sim(50,src.dist="U",param1=0,param2=1)
# sdm.sim(100,src.dist="U",param1=0,param2=1)
# sdm.sim(10,src.dist="X",param1=14)
# sdm.sim(50,src.dist="X",param1=14)
# sdm.sim(100,src.dist="X",param1=14)

