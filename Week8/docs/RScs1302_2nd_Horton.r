## RScs1302_2nd_Horton.r
# Transcribed from Chapter13 pdf by Eugene.Gallagher@umb.edu 2/3/13
library("Sleuth3")
summary(case1302)
with(case1302, interaction.plot(Company, Treat, Score))
# Two-way ANOVA, fit with multiple linear regression

lm1 = lm(Score ~ Company * Treat, data = case1302)
summary(lm1)

lm2 = lm(Score ~ Company + Treat, data = case1302)
summary(lm2) # Display 13.18 page 395

anova(lm1)
anova(lm2)
anova(lm2, lm1)

plot(lm2, which = 1)

# Randomization distribution -- doesn't work, don't know why   
obs = summary(lm(Score ~ Company + Treat, data = case1302))$coefficients["TREATPYGMALION",
          "t value"]
nulldist = do(15000) * summary(lm(Score ~ shuffle(Company) + shuffle(Treat),
   data = case1302))$coefficients["shuffle(Treat)PYGMALION", "t value"]
xhistogram(~result, groups = result >= obs, v = obs, data = nulldist)
# akin to Display 13.2
tally(~result >= obs, format = "proportion", data = nulldist)