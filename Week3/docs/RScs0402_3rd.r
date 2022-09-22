# RScs0402_3rd.r
# Copied from the R manual by Gallagher 1/16/13, revised 2/11/14, 2/12/14, 
# 9/21/2021
# Merged Sleuth3 package code with Horton's R code from his website: 
# http://www3.amherst.edu/~nhorton/sleuth/chapter04.pdf
# merged with Robison-Cox's Montana State University r code (website down)

library(Sleuth3)
## Options recommended by Horton
# install.packages("mosaic") # Horton's pacakge for teaching stats, only needs to be done once
require(mosaic) #needs to be done once per session
trellis.par.set (theme = col.mosaic ())
options(digits=3, show.signif.stars =FALSE)

str(case0402) # level 1 of Treatment rmis "Conventional" (1st alphabetically)
summary(case0402) 
favstats (Time ~ Treatment,data = case0402)
attach(case0402)

boxplot(Time ~ Treatment, main="Case 4.2") 
bwplot (Treatment  ~    Time,  data  = case0402) #mosaic boxplot
# A really misleading graphic:
densityplot (~Time,groups= Treatment,auto.key=TRUE,data= case0402)

median(Time[Treatment=="Conventional"])-median(Time[Treatment=="Modified"])  

wilcox.test(Time ~ Treatment, exact=FALSE, correct=TRUE, 
            alternative="greater")  # Rank-sum test; alternative: group 1 is greater
wilcox.test(Time ~ Treatment, exact=FALSE, correct=TRUE, 
            alternative="two.sided", conf.int=TRUE)  # Use 2-sided to get confidence int.  

# Note errors with exact tests:
wilcox.test(Time~Treatment, conf.int =   TRUE , exact =   TRUE , data = case0402)

# Robison-Cox MSU R code. Just perform a t test after transforming to ranks
case0402$rank <- rank(case0402$Time, ties="average")
t.test(rank ~Treatment,case0402,var.equal=TRUE)

## DOT PLOTS FOR PRESENTATION 
xTreatment    <- ifelse(Treatment=="Conventional",1,2) # Make numerical values  
myPointCode   <- ifelse(Censored==0,21,24)
# Changed the y limits from the Sleuth3 code:
plot(Time ~ jitter(xTreatment,.2),   # Jitter the 1's and 2's for visibility
     ylab="Completion Time (Sec.)",  xlab="Training Method (jittered)",
     main="Test Completion Times from Cognitive Load Experiment",
     axes=FALSE, pch=myPointCode, bg="green", cex=2, xlim=c(.5,2.5),
     ylim=c(50,300))  
axis(2) # Draw y-axis as usual
axis(1, tick=FALSE,  at=c(1,2),  # Draw x-axis without ticks
     labels=c("Conventional (n=14 Students)","Modified (n=14 Students)") )  
legend(1.5,300, legend=c("Did not Complete in 300 sec","Completed in 300 sec."),
       pch=c(24,21), pt.cex=2, pt.bg="green")  

detach(case0402)