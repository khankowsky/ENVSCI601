# Titanic
# from Frank Harrell's code
# written by Eugene.Gallagher@umb.edju 11/5/22 20:00, done 22:25, 2:25
require(rms)
options (prType ='latex') # for print, summary, anova
getHdata (titanic3) # get dataset from website
# List of names of variables to analyze
v  <- c('pclass','survived','age','sex','sibsp','parch')
t3 <- titanic3 [, v]
units(t3$age) <- 'years '
# latex(describe (t3), file ='')  # Couldn't get latex to work
describe(t3, file ='')

dd <-  datadist (t3)
# describe distributions of variables to rms
options ( datadist ='dd')
s <-  summary (survived ~ age + sex + pclass +
                cut2 (sibsp ,0:3) + cut2 (parch ,0:3) , data =t3)
plot (s, main ='', subtitles = FALSE ) # Figure 9.1
# Figure 9.1: Univariable summaries of Titanic survival

# Show 4-way relationships after collapsing levels. Suppress estimates based on
# < 25 passengers.
tn  <- transform (t3 ,
                agec = ifelse (age < 21, 'child', 'adult'),
                sibsp = ifelse ( sibsp == 0, 'no sib/sp', 'sib/sp'),
                parch = ifelse ( parch == 0, 'no par/ child', 'par/child'))
g <-  function (y) if( length (y) < 25) NA else mean (y)
s <-  with (tn , summarize ( survived,
                           llist (agec, sex, pclass, sibsp, parch), g))
# llist , summarize in Hmisc package
# Figure 9.2:
ggplot ( subset (s, agec != 'NA '),
         aes(x= survived , y=pclass, shape =sex)) +
  geom_point () + facet_grid ( agec ~ sibsp * parch ) +
  xlab ('Proportion Surviving') + ylab ('Passenger Class') +
  scale_x_continuous (breaks =c(0, .5 , 1))
# Figure 9.2: Multi-way summary of Titanic survival

# Exploring Trends with Nonparametric Regression
# Figure 9.3
b <-  scale_size_discrete ( range =c(.1 , .85))

yl  <- ylab ( NULL )
p1  <- ggplot (t3 , aes(x=age , y= survived )) +
  histSpikeg ( survived ~ age , lowess =TRUE , data =t3) +
  ylim (0 ,1) + yl
p2  <- ggplot (t3 , aes(x=age , y= survived , color =sex)) +
  histSpikeg ( survived ~ age + sex , lowess =TRUE ,
               data =t3) + ylim (0 ,1) + yl
p3  <- ggplot (t3 , aes(x=age , y= survived , size = pclass )) +
  histSpikeg ( survived ~ age + pclass , lowess =TRUE ,
               data =t3) + b + ylim (0 ,1) + yl
p4  <- ggplot (t3 , aes(x=age , y= survived , color =sex ,
                      size = pclass )) +
  histSpikeg ( survived ~ age + sex + pclass ,
               lowess =TRUE , data =t3) +
  b + ylim (0, 1) + yl
gridExtra :: grid.arrange (p1, p2, p3, p4, ncol =2) # combine 4

# Figure 9.3: Nonparametric regression (loess) estimates of the relationship
# between age and the probability of surviving the Titanic, with tick marks
# depicting the age distribution. The top left panel shows unstratified
# estimates of the probability of survival. Other panels show nonparametric 
# estimates by various stratifications.

# Figure 9.4
top <-theme ( legend.position ='top')
p1 <-   ggplot (t3 , aes(x=age , y= survived , color = cut2(sibsp,
              0:2) )) + stat_plsmo () + b + ylim (0, 1) + yl + top +
              scale_color_discrete (name ='siblings/spouses')
p2  <- ggplot (t3 , aes(x=age , y= survived , color = cut2 (parch,
              0:2) )) + stat_plsmo () + b + ylim (0 ,1) + yl + top +
             scale_color_discrete (name ='parents/children')
gridExtra :: grid.arrange (p1, p2 , ncol =2)
# Figure 9.4: Relationship between age and survival stratified by the number of
# siblings or spouses on board (left panel) or by the number of parents or
# children of the passenger on board (right panel).

# First fit a model that is saturated with respect to age, sex, pclass.
# Insuffcient variation in sibsp, parch to fit complex interactions or
# nonlinearities.
f1  <-  lrm(survived ~ sex * pclass * rcs(age, 5) +
            rcs(age, 5) * (sibsp + parch), data = t3) # Table 9.1
#print(anova (f1), table.env = TRUE, label ='titanic - anova3', size ='small')
# anova(f1)  # can't find the error in these lines

# 3-way interactions, parch clearly insignificant, so drop
f <- lrm( survived ~ (sex + pclass + rcs(age, 5))^2 +
           rcs(age,5)*sibsp, data =t3)
# CHAPTER 9. LOGISTIC MODEL CASE STUDY: SURVIVAL OF TITANIC PASSENGERS 9-9
print (f) # finding errors
f
f <-lrm(formula = survived ~ (sex + pclass + rcs(age, 5))^2 + rcs(age,5) * sibsp,
    data = t3)
print(f)  # errors in transl??

print(anova(f), table.env =TRUE, label ='titanic-anova2', size ='small') #9.2
f
p <-  Predict (f,age, sex, pclass, sibsp =0, fun= plogis)
ggplot (p) # Fig. 9.5

f <-  update (f, x=TRUE , y= TRUE )
# x = T R U E , y = TRUE adds raw data to fit object so can bootstrap
set.seed (131) # s o c a n r e p l i c a t e r e - s a m p l e s
latex (validate (f, B =200) , digits =2, size ='Ssize ')  #error

cal <-calibrate (f, B =200) # F i g u r e 9.7
plot (cal , subtitles = FALSE )          

# Examining Missing Data Patterns

na.patterns <-  naclus (titanic3)
require ( rpart ) #Recursive partitioning package

who.na  <- rpart ( is.na (age) ~ sex + pclass + survived +
                   sibsp + parch , data = titanic3 , minbucket =15)
naplot ( na.patterns , 'na per var ')
plot (who.na , margin =.1); text ( who.na ) # F i g u r e 9.8
plot ( na.patterns )

plot ( summary ( is.na (age) ~ sex + pclass + survived +
                   sibsp + parch , data =t3)) # F i g u r e 9.9

m <-  lrm( is.na (age) ~ sex * pclass + survived + sibsp + parch ,
         data =t3)
print (m, needspace ='3 .5in ')  # error in print command

lrm(formula = is.na(age) ~ sex * pclass + survived + sibsp +
      parch, data = t3)  # error in transl

print (anova (m), table.env =TRUE , label =' titanic-anova.na ') # T a b l e 9.3
 # error in above print statement

#First try: conditional mean imputation
#Default spline transformation for age caused distribution of imputed values to
# be much different from non-imputed ones; constrain to linear
xtrans <-   transcan (~ I(age) + sex + pclass + sibsp + parch ,
                    imputed =TRUE , pl=FALSE , pr=FALSE , data =t3)

summary ( xtrans )


# Look at mean imputed values by sex , pclass and observed means
# age . iis age , filled in with conditional mean estimates
age.i <-  with (t3 , impute (xtrans , age , data =t3))
i <-  is.imputed ( age.i )
with (t3 , tapply ( age.i [i], list (sex[i], pclass [i]) , mean ))

with (t3 , tapply (age , list (sex , pclass ), mean , na.rm = TRUE ))

dd  <- datadist (dd , age.i )
f.si <-   lrm( survived ~ (sex + pclass + rcs(age.i ,5))^2 +
              rcs(age.i ,5)*sibsp , data =t3)
print (f.si, coefs = FALSE)  # error in the print function

lrm(formula = survived ~ (sex + pclass + rcs(age.i, 5))^2 + 
      rcs(age.i,5) * sibsp, data = t3)

p1 <- Predict (f, age , pclass , sex , sibsp =0, fun= plogis )
p2 <- Predict (f.si , age.i , pclass , sex , sibsp =0, fun= plogis )
p <-  rbind ('Casewise Deletion '= p1 , 'Single Imputation '=p2,
           rename =c(age.i ='age')) # creates.set.variable
ggplot (p, groups ='sex', ylab ='Probability of Surviving')
# Figure 9.10

print (anova (f.si), table.env =TRUE,label ='titanic-anova.si') #Table 9.4 # error

# Multiple imputation

set.seed (17) # s o c a n r e p r o d u c e r a n d o m a s p e c t s
mi  <- aregImpute (~ age + sex + pclass +
                    sibsp + parch + survived ,
                  data =t3 , n.impute =20 , nk =4, pr= FALSE )
mi
# Print the first 10 imputations for the first 10 passengers
# CHAPTER 9. LOGISTIC MODEL CASE STUDY: SURVIVAL OF TITANIC PASSENGERS 9-21
# having missing age
mi$imputed$age [1:10 , 1:10]

#Show the distribution of imputed (black) and actual ages (gray).

plot (mi)
Ecdf (t3$age , add=TRUE , col='gray ', lwd =2,
      subtitles = FALSE ) # Fig . 9.11

f.mi <-   fit.mult.impute (
  survived ~ (sex + pclass + rcs(age,5))^2 +
    rcs(age ,5)*sibsp,
  lrm , mi , data =t3 , pr= FALSE )
print (anova (f.mi), table.env =TRUE , label =' titanic-anova.mi ',
        size ='small') # Table 9.5 # invalid replacement argument

# Show estimated eects of age by classes.
p1 <- Predict (f.si , age.i, pclass, sex , sibsp =0, fun= plogis )
p2  <- Predict (f.mi, age, pclass, sex , sibsp =0, fun= plogis )
p <-  rbind ('Single Imputation' =p1 , 'Multiple Imputation'=p2 ,
           rename =c(age.i ='age'))
ggplot (p, groups ='sex', ylab ='Probability of Surviving ')
# Figure 9.12

# Summarizing the fitted model

# Get predicted values for certainty pes of passengers
s <-  summary (f.mi , age=c(1 ,30) , sibsp =0:1)
# o v e r r i d e d e f a u l t r a n g e s f o r 3 v a r i a b l e s
plot (s, log=TRUE , main ='') # F i g u r e 9.13

phat <-  predict (f.mi,
                combos  <-
                expand.grid (age=c(2 ,21 ,50),sex= levels (t3$sex),
                             pclass = levels (t3$ pclass ),
                             sibsp =0) , type ='fitted')
# Can also use Predict (f.m i, age = c(2 , 21 , 50 ), sex , pclass,
# sibsp = 0 , fun = plogis) $ y h a t
options (digits =1)
data.frame(combos, phat)

pred.logit  <- Function ( f.mi )
# Note: if don't defines ibsp to pred.logit, defaults to 0
# normally just type the function name tosee its body
latex ( pred.logit , file ='', type ='Sinput', size ='small',
        width.cutoff =49)

# Run the newly created function
plogis ( pred.logit (age=c(2 ,21 ,50) , sex='male ', pclass ='3rd ')) # Not correct

