# RScs0501_3rd.r
# Code copied from Sleuth3 manual
# by Eugene.Gallagher@umb.edu
# References:
# Box, G. E. P., J. S. Hunter, and W. G. Hunter. 2005. Statistics for 
#     experimenters: design, innovation and discovery, 2nd Edition. 
#     Wiley-Interscience, Hoboken NJ. 639 pp.
# Ramsey, F. L. and D. W. Schafer. 2013. The statistical sleuth: a course in
#     methods of data analysis, 3rd Edition. Brooks/Cole Cengage Learning,
#     Boston MA, 760 pp.
# Graphical ANOVA from Box et al. (2005)  added 4/1/13, 
# revised 2/18/14, 2/28/14, 3/9/15, 9/22/21, 9/25/21, 9/28/21

# Install packages, many required for multcomp alone
library(agricolae) # For the Duncan's test
library(BHH2)     # Box et al. (2005)
library(car)      # for Boxplot To label outliers, not really necessary here.\
library(mosaic)
library(multcomp)
library(mvtnorm)
library(MASS)
library(Sleuth3)
library(survival)
library(TH.data)

str(case0501)
favstats(Lifetime ~ Diet, data = case0501)  # from Horton's mosaic

attach(case0501)
data(case0501)

# Re-order levels for better boxplot organization:

myDiet <- factor(case0501$Diet, levels=c("NP","N/N85","N/R50","R/R50","lopro","N/R40") )
myNames <- c("NP(49)","N/N85(57)","N/R50(71)","R/R50(56)","lopro(56)",
             "N/R40(60)") # Make these for boxplot labeling.
boxplot(Lifetime ~ myDiet, ylab= "Lifetime (months)", names=myNames,
        xlab="Treatment (and sample size)", data=case0501)
densityplot(~Lifetime, groups = Diet, auto.key = TRUE, data=case0501)

# car boxplot
Boxplot(Lifetime ~ myDiet, ylab= "Lifetime (months)", names=myNames,
        main= "Lifetimes of Mice on 6 Diet Regimens",
        xlab="Diet (and sample size)", col="green", boxlwd=2, medlwd=2, whisklty=1,
        whisklwd=2, staplewex=.2, staplelwd=2, outlwd=2, outpch=21, outbg="green",
        outcex=1.5)

myAov1 <- aov(Lifetime ~ Diet, data = case0501) # One-way analysis of variance
myAov1
summary(myAov1)
summary.lm(myAov1)

attributes(myAov1)
# This plot is not appropriate since the contrasts were hypothesized a priori,
# and the 95% CI's will be tighter given these a priori contrasts

# Analyze the resdual plot 

# Residual vs. fitted plot
plot(myAov1, which=1) # Plot residuals versus estimated means.
summary(myAov1)
# This plot indicates no problems.

# Weindruch et al. (1986) didn't set 5 a priori hypotheses as described in 
# Sleuth3. They used Duncan's multiple comparison procedure, available in the
# agricolae package (installed and loaded above)
out <- duncan.test(myAov1,"Diet", 
                   main="Sleuth3 Case Study 5.1, Duncan's multiple range test")
str(out)
plot(out,variation="IQR")
duncan.test(myAov1,"Diet",alpha=0.05,console=TRUE)
# The pattern of results at alpha=0.05 matches the pairwise t tests with no
# corrections

TukeyHSD(myAov1)
plot(TukeyHSD(myAov1))

# Box et al. (2005) Graphical ANOVA see Display 5.16, 5.17, p 134 & 135
anovaPlot(myAov1, stacked = TRUE, base = TRUE, axes = TRUE,
          faclab = TRUE, labels = TRUE, cex = par("cex"),
          cex.lab = par("cex.lab"))
# This shows the strong differences between most of the diets, especially NP and
# N/R40

# All Pairwise t tests with pooled sd, as in Ramsey & Schafer (2013)
pairwise.t.test(Lifetime,Diet, pool.SD=TRUE, p.adj="none", data=case0501) # All t-tests

# From the Sleuth3 vignette
## p-VALUES AND CONFIDENCE INTERVALS FOR SPECIFIED COMPARISONS OF MEANS
if(require(multcomp)){
  diet    <- factor(Diet,labels=c("NN85", "NR40", "NR50", "NP", "RR50", "lopro")) 
  myAov2  <- aov(Lifetime ~ diet - 1) 
  myComparisons <- glht(myAov2,
                        linfct=c("dietNR50 - dietNN85 = 0", 
                                 "dietRR50  - dietNR50 = 0",
                                 "dietNR40  - dietNR50 = 0",
                                 "dietlopro - dietNR50 = 0",
                                 "dietNN85  - dietNP   = 0")   ) 
  summary(myComparisons,test=adjusted("none")) # No multiple comparison adjust.
  confint(myComparisons, calpha = univariate_calpha()) # No adjustment
}

### Or another approach from glht vignette 
amod <- aov(Lifetime~diet, data = case0501)
# For each contrast, the sum of the absolute values for each side of the
# contrast should be 1 to get the proper effect size.
contr <- rbind("dietNR50 - dietNN85" = c(-1,0,1,0,0,0),
               "dietNR40 - dietNR50" = c(0,1,-1,0,0,0),
               "dietRR50 - dietNR50" = c(0,0,-1,0,1,0),
               "dietlopro - dietNR50" = c(0,0,-1,0,0,1),
               "dietNN85 - dietNP" =  c(1,0,0,-1,0,0))
out<-glht(amod, linfct = mcp(diet = contr))
out
confint(out)

# Both approaches produce identical answers, which match the Sleuth text



## EXAMPLE 5: BOXPLOTS FOR PRESENTATION
boxplot(Lifetime ~ myDiet, ylab= "Lifetime (months)", names=myNames,
        main= "Lifetimes of Mice on 6 Diet Regimens",
        xlab="Diet (and sample size)", col="green", boxlwd=2, medlwd=2, whisklty=1,
        whisklwd=2, staplewex=.2, staplelwd=2, outlwd=2, outpch=21, outbg="green",
        outcex=1.5)

detach(case0501)