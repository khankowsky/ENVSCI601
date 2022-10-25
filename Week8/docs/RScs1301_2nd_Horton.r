## RScs1301_2nd_Horton.r
# Horton code for case study 13.1, from pdf chapter13
% Transcribed by Eugene.Gallagher@umb.edu 2/3/2013

library("Sleuth3")
install.packages("mosaic") # note the quotation marks
# Once this is installed, it can be loaded by running the command:
require(mosaic)
# This needs to be done once per session.
# We also set some options to improve legibility of graphs and output.
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 4, show.signif.stars = FALSE)

case1301$logitcover = with(case1301, log(Cover/(100 - Cover)))
summary(case1301)
favstats(logitcover ~ Treat, data = case1301)
with(case1301, interaction.plot(Block, Treat, Cover))
plot(aov(Cover ~ Block * Treat, data = case1301), which = 1)
with(case1301, interaction.plot(Block, Treat, logitcover))
anova(lm(logitcover ~ Block * Treat, data = case1301))
anova(lm(logitcover ~ Block + Treat, data = case1301))
plot(aov(logitcover ~ Block + Treat, data = case1301), which = 1)
case1301$resid = predict(aov(logitcover ~ Block + Treat, data = case1301))
xhistogram(~resid, fit = "normal", data = case1301)
plot(aov(logitcover ~ Block + Treat, data = case1301), which = 3)
plot(aov(logitcover ~ Block + Treat, data = case1301), which = 4)
case1301[c(13, 22, 87), ]
# Linear combinations, see Display 13.12 in 3rd ed p 398
model.tables(aov(lm(logitcover ~ Block * Treat, data = case1301)), type = "effects")
model.tables(aov(lm(logitcover ~ Block * Treat, data = case1301)), type = "means")

library(gmodels)

lm1 = lm(logitcover ~ Treat + Block, data = case1301)
coef(lm1)
large = rbind(`Large fish` = c(0, -1/2, 1/2, 0, -1/2, 1/2))
small = rbind(`Small fish` = c(-1/2, 1/2, 0, -1/2, 1/2, 0))
limpets = rbind(Limpets = c(-1/3, -1/3, -1/3, 1/3, 1/3, 1/3))
limpetsSmall = rbind(`Limpets X Small` = c(1, -1/2, -1/2, -1, 1/2, 1/2))
limpetsLarge = rbind(`Limpets X Large` = c(0, 1, -1, 0, -1, 1))
fit.contrast(lm1, "Treat", large, conf.int = 0.95)
fit.contrast(lm1, "Treat", small, conf.int = 0.95)
fit.contrast(lm1, "Treat", limpets, conf.int = 0.95)
fit.contrast(lm1, "Treat", limpetsSmall, conf.int = 0.95)
fit.contrast(lm1, "Treat", limpetsLarge, conf.int = 0.95)

