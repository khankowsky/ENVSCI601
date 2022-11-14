#ELM12GS

# Analysis of Elinder & Erixson data on 18 maritime disasters from:
# Elinder, M & O Erixson. 2012. Gender, social norms, and survival in maritime
# disasters. Proceedings of the National Academy of Sciences, 109: 13220-13224.
# Code adapted from 
# Harrell, F. E. 2015. Regression Modeling Strategies, 2nd edition. Springer,
#   New York. 582 p. and
# Harrell, F. D. 2021. Regression Modeling Strategies. Course Notes. May 2021.
# Code modified by Eugene.Gallagher@umb.edu and 18 spreadsheets provided by 
# Elinder to Gallagher in January 2013. Gallagher compiled them into elm12gs.csv
# # Adapted by Eugene.Gallagher@umb.edu, 11/24/21

library(tidyverse) # provides ggplot package and other packages
# Frank Harrell's two essential packages:
library(Hmisc)
require(rms)

elm12gs<-read.csv("../data/elm12gs.csv")
str(elm12gs) # 15459 observations of 17 variables
describe(elm12gs)
elm12gs2 <- elm12gs %>% #make a new dataframe that gets rid of all NA values
  drop_na()
str(elm12gs2)  # 3509 observations left, too many cases lost,
# and I don't know how to do data imputation yet
# remove only rows lacking either age or gender
elm12gs3<- elm12gs[is.na(elm12gs$Gender)+is.na(elm12gs$Age) == 0, ] 
str(elm12gs3) #6826 observations
# remove only rows lacking either Survival or Passenger Class
elm12gs4<-elm12gs3[is.na(elm12gs3$Survival)+is.na(elm12gs3$PassengerClass) == 0, ] 
str(elm12gs4) # Now 3511 observations
# remove 5 cases with 4th class passengers
elm12gs5 <-elm12gs4[elm12gs4$PassengerClass != 4, ]
str(elm12gs5) #3506 observations left
# List of variables to analyze
v <-c('ShipId','WomeNAdChildrenFirst','Age','Gender','PassengerClass',
      'No_OfPassengers', 'No_OfWomenPassengers', 'Quick', 'Year','Survival')
d<-elm12gs5[, v]
str(d)
passengerclass<-factor(d$PassengerClass)
shipid<-factor(d$ShipId)
units(d$Age) <- "years"
describe(d) # Note 5 4th class passengers deleted

myGlm1<-glm(Survival~shipid+Gender+Age+factor(passengerclass),data=d)
summary(myGlm1)

dd <-datadist(d)
options <-( datadist ='dd')
s <-summary(Survival ~ Age + Gender + PassengerClass, data =d)
plot(s, main ='', subtitles = FALSE ) # Figure 9.1
tn  <- transform (d,agec = ifelse (Age < 21, 'child', 'adult'))
tn$sex<-ifelse(d$Gender==0,"M","F")
d$sex<-tn$sex
g <-  function (y) if( length (y) < 25) NA else mean (y)
s <-  with (tn , Hmisc::summarize (Survival, llist (agec, Gender , sex, PassengerClass), g))

ggplot (subset (s, agec != 'NA'),
        aes(x= Survival , y=PassengerClass, shape = sex)) +
  geom_point () + facet_grid (agec ~ PassengerClass) +
  xlab ('Proportion Surviving ') + ylab ('Passenger Class') +
  scale_x_continuous ( breaks =c(0, .5 , 1))

# Exploring trends with nonparametric regression
b <-  scale_size_discrete ( range =c(.1 , .85))
yl <-  ylab ( NULL )
p1 <-  ggplot (d , aes(x=Age , y= Survival )) +
  histSpikeg (Survival ~ Age , lowess =TRUE , data =d) +
  ylim (0 ,1) + yl
p2  <- ggplot (d , aes(x=Age , y= Survival , color =sex)) +
  histSpikeg ( Survival ~ Age + sex , lowess =TRUE ,
               data =d) + ylim (0 ,1) + yl
p3  <- ggplot (d , aes(x=Age , y= Survival , size = PassengerClass )) +
  histSpikeg ( Survival ~ Age + PassengerClass , lowess =TRUE ,
               data =d) + b + ylim (0 ,1) + yl
p4  <- ggplot (d , aes(x=Age , y= Survival , color =sex ,
                       size = PassengerClass)) +
  histSpikeg ( Survival ~ Age + sex + PassengerClass ,
               lowess =TRUE , data =d) +
  b + ylim (0 ,1) + yl
gridExtra::grid.arrange (p1 , p2 , p3 , p4 , ncol =2) # combine 4

# Add Ship ID
yl <-  ylab ( NULL )
p1 <-  ggplot (d , aes(x=Age , y= Survival )) +
  histSpikeg (Survival ~ Age , lowess =TRUE , data =d) +
  ylim (0 ,1) + yl
p2  <- ggplot (d , aes(x=Age , y= Survival , color =sex)) +
  histSpikeg ( Survival ~ Age + sex , lowess =TRUE ,
               data =d) + ylim (0 ,1) + yl
p3  <- ggplot (d , aes(x=Age , y= Survival , size = PassengerClass )) +
  histSpikeg ( Survival ~ Age + PassengerClass , lowess =TRUE ,
               data =d) + b + ylim (0 ,1) + yl
p4  <- ggplot (d , aes(x=Age , y= Survival , color =sex ,
                       size = PassengerClass)) +
  histSpikeg ( Survival ~ Age + sex + PassengerClass,
               lowess =TRUE , data =d) +
  b + ylim (0 ,1) + yl
p5  <- ggplot (d , aes(x=Age , y= Survival , color =ShipId ,
                       size = sex)) +
  histSpikeg ( Survival ~ Age + sex + ShipId ,
               lowess =TRUE , data =d) +
  b + ylim (0 ,1) + yl
gridExtra::grid.arrange (p1 , p2 , p3 , p4 , p5, ncol =2) # combine 5


# Do full glm using Harrell's lrm (logistic regression model):
d$shipid<-factor(d$ShipId)
f1 <- lrm(Survival ~ PassengerClass * rcs(Age ,5) +
            shipid*sex, data =d) # Table 9.1
print (anova (f1), table.env =TRUE , label ='Maritime Disasters-anova1 ', size ='small ')

f2 <- lrm(Survival ~ PassengerClass + rcs(Age ,5) +
            shipid*sex, data =d) # Table 9.1
print (anova (f2), table.env =TRUE , label ='Maritime Disasters-anova1 ', size ='small ')
f3 <- lrm(Survival ~ PassengerClass + rcs(Age ,5) + rcs(Age,5)^2 +
            shipid*sex, data =d) # Table 9.1
print ( anova (f3), table.env =TRUE , label ='Maritime Disasters ',size ='small ') #9.2

dd <-datadist(d)
options ( datadist ="dd")
p <- Predict (f3, Age, sex , PassengerClass, shipid, fun= plogis)
ggplot(p) # It worked! 11/24/21 6:22 PM (working 4+ hours, datadist was the problem)


p1  <- Predict (f3, Age,  sex, PassengerClass, shipid=7, fun= plogis)
p2  <- Predict (f3, Age , sex, PassengerClass, shipid=8, fun= plogis )
p3 <-  Predict (f3, Age , sex, PassengerClass, shipid=10, fun= plogis)
p4 <-  Predict (f3, Age , sex, PassengerClass, shipid=11, fun= plogis)
p <-  rbind ('Norge'=p1 , 'Titanic'=p2, 'Lusitania'=p3 , 
             'Principessa Mafalda'=p4) 
# creates.set.variable
ggplot (p, groups ='sex', ylab ='Probability of Surviving ')

# Sent the code up to here to Frank Harrell on 11/24/21. He responded.
# Later on 11/24/21, I noticed the Titanic patterns were markedly different from
# Harrell's. He never plotted the full data set, only men and women without
# spouses or children in Figure 9.5. Decided to add a few more interactions to
# see if the patterns would match Harrell's. They don't
f4 <- lrm(Survival ~ PassengerClass + rcs(Age ,5):shipid + rcs(Age,5)^2  +
            shipid*sex, data =d) # Table 9.1
summary(f4)
p5  <- Predict (f4, Age , sex, PassengerClass, shipid=7, fun= plogis )
p6  <- Predict (f4, Age , sex, PassengerClass, shipid=8, fun= plogis )
p7  <- Predict (f4, Age , sex, PassengerClass, shipid=10, fun= plogis )
p8  <- Predict (f4, Age , sex, PassengerClass, shipid=11, fun= plogis )
P <-  rbind ('Norge'=p5 , 'Titanic'=p6, 'Lusitania'=p7 ,
             'Principessa Mafalda'=p8) 
# creates.set.variable
ggplot (P, groups ='sex', ylab ='Probability of Surviving ')
ggplot (P, groups ='shipid', ylab ='Probability of Surviving ')
ggplot (P, groups ='PassengerClass', ylab ='Probability of Surviving ')


str(elm12gs) # 15459 observations of 17 variables
describe(elm12gs)
attach(elm12gs)
# p<-plot(elm12gs) plots but Too busy
f <- summaryM(WomeNAdChildrenFirst + Quick + Crew + Gender ~ Survival + ShipId,
              groups='Survival', test=TRUE)
# datadist is needed for plots and Harrell Tables.
dd <-datadist(elm12gs)
options <-(datadist ='dd ')

f  # Confusing table

## Analyze Hypothesis 1. (H1) Do women have an advantage over men in maritime
# disasters analyzed with the full data set
H1 <- lrm(Survival ~ Gender, data =elm12gs)
H1
summary(H1)
print (anova (H1), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
# Women survival odds are 27% less than men (95% CI 22% to 32%, p<0.0001)

## Analyze hypothesis 2 crew vs. Passenger with the full dataset
H2 <- lrm(Survival ~ Crew, data =elm12gs) # Table 9.1
H2
dd <-datadist(elm12gs)
options <-(datadist ='dd ')
summary(H2)
print (anova (H2), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
# Women survival odds are 13% less than men (95% CI 6% to 19%)
# Crew survival odds are 1.96 times that of passengers (95% CI 1.81 to 2.11)

# Hypothesis 3 WCF order improves the odds of women surviving
H3 <- lrm(Survival ~ WomeNAdChildrenFirst*Gender, data =elm12gs)
H3
summary(H3)
exp(1.6674)
exp(1.6674+cbind(1,-1)*0.0816*qnorm(.975))
print (anova (H3), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
# A Women & Children's first order was issued only 5 times. Odds of men 
# surviving decline by 53% if a WCF order is issued (95% CI 49% to 57%,
# p <0.00001). A WCF order produces an effect of women increasing their odds of
# survival relative to men 5.3 times (95% CI 4.5% to 6.2%, p < 0.0001. Overall,
# women have odds of survival 57% lower than men (95% CI, 53% to 61%, p <0.0001). 

# H4) Do women fare worse if a ship sinks quickly?
H4 <- lrm(Survival ~ Quick*Gender, data =elm12gs)
H4
summary(H4)
print (anova (H4), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
# The odds of a women surviving are 21% less than men (95% CI 12% to 29%, p <0.0001)
# The odds of a woman surviving decline by 15% if the ship sinks quickly (p = 0.03)

# H5) Do the women's odds of survival improve when they are scarce?
H5 <- lrm(Survival ~ WomenPassengers_passengers*Gender, data =elm12gs)
H5
summary(H5)
print (anova (H5), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
# The odds of a women surviving are 17% less than men (95% CI 3% to 11%, p <0.0001)
# The odds of a woman surviving decline with increasing percentages of women (p < 0.0001)

#H6) Do women's odds of survival improve if the voyage is longer than 1 day?
elm12gs$long <- ifelse(elm12gs$LengthOfVoyage>1,1,0)
dd <-datadist(elm12gs)
options <-( datadist ='dd')
H6 <- lrm(Survival ~ long*Gender, data =elm12gs)
H6
summary(H6)
print (anova (H6), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
exp(c(-.3864,-.3030,-.1066))
1-exp(c(-.3864,-.3030,-.1066))
1-exp(-.1066 + qnorm(.975)*c(-1,1) * 0.0923)

# [1] 0.3205013 0.2614009 0.1011148
# The odds of men surviving on a long voyage are 32% less than on a short voyage 
# (95% CI 24% to 40%, p <0.0001)
# The odds of a women surviving are 26% less than men (95% CI 13% to 37%, p <0.0002)
# The odds of a woman surviving decline 10% on a long relative to a short 
# voyage, but the effect is uncertain (95% CI -7% to 25%, p < 0.25)

#H601) Do women's odds of survival improve if the ship is small (<686 passengers)?
# (<686 passengers
elm12gs$small <- ifelse(elm12gs$No_OfPassengers<686,1,0)
dd <-datadist(elm12gs)
options <-( datadist ='dd')
H601 <- lrm(Survival ~ small*Gender, data =elm12gs)
H601
summary(H601)
print (anova (H601), table.env =TRUE , label ='Maritime Disasters ',size ='small ')
1-exp(c(-.4776,-.2756,-.2845, -.0290))
# [1] 0.37972974 0.24088348 0.24760964 0.02858354
1-exp(-.0290 + qnorm(.975)*c(-1,1) * 0.1048)
# [1]  0.2089580 -0.1929202

# The odds of men surviving on a small ship are 32% greater than on a large ship 
# (95% CI 19% to 49%, p <0.0001)
# The odds of a women surviving are 25% less than men (95% CI 19% to 31%, p <0.0001)
# The odds of a woman surviving decline 3% relative to men on a small relative 
# to a large ship, voyage, but the effect is uncertain (95% CI -19% to 21%, p = 0.78)

detach(elm12gs)


