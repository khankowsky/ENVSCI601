# RScs0202_3rd.r
# Transcribed by Eugene Gallagher, last modified 2/5/14, 2/13/14, 9/12/21
# Note that there is an error in R's calculation of the Wilcoxon
# signed rank statistic (Ch 4) on this problem. This is due to the
# way processors handle double precision data, corrected by SPSS &
# Matlab, not R
# written some time about 2012 (largely from Sleuth3 vignette), revised 9/15/21

library(ggformula) # graphics, a part of the tidyverse
library(dplyr)     # data manipulation
library(mosaic)    # Nick Horton's package
library(Sleuth3)   # Sleuth data sets

str(case0202)

data(case0202, package="Sleuth3")
str(case0202)

# Adam Loy tidyverse summary
summary(case0202)

# Nick horton's analysis of the differences
diff = case0202[, "Unaffected"] - case0202[, "Affected"]
favstats(diff)

# Stem and leaf plot
with(case0202, stem(Unaffected-Affected, scale=2))

t.test(diff) # Paired t-test is a one-sample t-test on differences

t.test(Unaffected, Affected, pair=TRUE, data=case0202)  # Alternative coding for the same test

boxplot(diff,       
        ylab="Difference in Hippocampus Volume (cubic cm)", 
        xlab="15 Sets of Twins, One Affected with Schizophrenia", 
        main="Hippocampus Difference: Unaffected Twin Minus Affected Twin",
        data=case0202) 
        abline(h=0,lty=2)   # Draw a dashed (lty=2) horizontal line at 0    


## BOXPLOT FOR PRESENTATION:
boxplot(diff, 
        ylab="Difference in Hippocampus Volume (cubic cm)", 
        xlab="15 Sets of Twins, One Affected with Schizophrenia",
        main="Hippocampus Difference: Unaffected Minus Affected Twin",  
        col="green", boxlwd=2, medlwd=2, whisklty=1, whisklwd=2, 
        staplewex=.2, staplelwd=2, outlwd=2, outpch=21, outbg="green",
        outcex=1.5)      
abline(h=0,lty=2) 






