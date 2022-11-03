# RScs1801_3rd.r
# Code from the Sleuth3 vignette modified by Eugene.Gallagher@umb.edu 11/12/21
# 11/14/21 Added gmodels::CrossTable
# Most of the code from the Sleuth3 help file.
library(gmodels)
library(Hmisc)
library(Sleuth3)
str(case1801)
attach(case1801)

## EXPLORATION
# The data are available in Sleuth3 case 1801 data, but it's easy to enter them
myTable <- matrix(c(16,2045,7,1044), nrow = 2, ncol = 2, byrow = TRUE,
                  dimnames = list(c("Obese", "Not Obese"),c("Yes", "No")))
myTable   # Show the table

## INFERENCE (4 methods for getting p-values and confidence intervals)
prop.test(myTable, alternative="greater", correct=FALSE) # Compare 2 proportions
prop.test(myTable, alternative="greater", correct=TRUE) # ...with Yates continuity correctio  
prop.test(myTable,correct=TRUE) # 2-sided alternative (default) to get CI
chisq.test(myTable) # Pearson's Chi-Squared Test
fisher.test(myTable, alternative="greater") # Fisher's exact test  
fisher.test(myTable) # 2-sided alternative to get CI for odds ratio

myGlm1  <- glm(myTable ~ Obesity, family=binomial)  # Logistic reg(Sleuth CH 21)
summary(myGlm1)  # Get p-value-- 0.734
beta    <- myGlm1$coef
exp(beta[2])  #Odds of death are estimated to be 17% higher for  obese women 
exp(confint(myGlm1,2)) # 95% confidence interval  

## Analyze the count data from the 2 x 2 contingency table.
## Display proportions by row
prop.table(myTable,1)  # The 1 tells R to calculate the proportions along rows

# Calculate the standard error for the difference.
N<-sum(myTable)
pc<-sum(Deaths)/N
n1<-sum(myTable[1,])
n2<- N-n1
p1<- myTable[1,1]/n1
p2<- myTable[2,1]/n2
SE_2p<-sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
SE_2p

## Calculate a 95% CI for the difference in proportions
CI<-(p1-p2)+c(-qnorm(.975)*SE_2p,qnorm(.975)*SE_2p)
CI
# The results should match 
pout<-prop.test(myTable, alternative="two.sided", correct=FALSE) # Compare 2 proportions
pout

# Calculate the standard error for equality on Sleuth page 556.
SE_o<-sqrt(pc*(1-pc)/n1+pc*(1-pc)/n2)
SE_o

## part b
z_stat<-(p2-p1)/SE_o
pnorm(z_stat)*2

## Compute the sample odds for the two proportions
odds2<-c(p1/(1-p1),p2/(1-p2))
odds2

## Odds ratio
oddsratio<-odds2[1]/odds2[2]
oddsratio

## Standard error of the log odds ratio
selor <- sqrt(1/(n1*p1*(1-p1))+1/(n2*p2*(1-p2)))
selor

## Confidence interval for the odds ratio:
lor<-log(oddsratio)
orCI<-exp(lor+(qnorm(0.975)*c(-selor,selor)))
orCI

## Analyze the data with a table like SPSS, includes warnings about small
# sample sizes
CrossTable(myTable,digits=3,fisher = TRUE, chisq = TRUE, expected = TRUE, 
           format = "SPSS")

# Statistical Summary
sprintf('The odds of cardiovascular death for the obese group are estimated to be')
sprintf('%4.2f times the odds of cardiovascular death for the non-obese group',oddsratio)
sprintf('(approximate 95%% CI: %4.2f to %4.2f).',orCI[1],orCI[2])

## GRAPHICAL DISPLAY FOR PRESENTATION
#        Deaths NonDeaths
#Obese        16      2045
#NotObese      7      1044
Po<-prop.test(myTable[1],n1) #For one proportion, est: 0.0078 95% CI: 0.0046 to 0.013
Po
Pno<-prop.test(myTable[2],n2) #For one proportion, est: 0067 95% CI: 0.0029 to 0.014
Pno
pHat    <- c(p1, p2)*1000 # Get estimated deaths per 1,000 women
lower95 <- c(Po$conf.int[1], Pno$conf.int[1])*1000
upper95 <- c(Po$conf.int[2], Pno$conf.int[2])*1000

myObj   <- Cbind(pHat,lower95,upper95) 
  Dotplot(Obesity ~ myObj,   # Draw a dot plot of estimates and CIs
          xlab="Estimated CVD Deaths Per 1,000 Women (and 95% Confidence Intervals)",
          ylab="Weight Category", ylim=c(.5,2.5), cex=2)

detach(case1801)
