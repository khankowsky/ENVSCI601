# RScs0801_2nd_Horton

install.packages("mosaic") # note the quotation marks
require(mosaic)
install.packages("Sleuth2") # note the quotation marks
require(Sleuth2)
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 4)

summary(case0802)

histogram(~Time, type = "density", density = TRUE, nint = 10, data = case0802)
case0802$logtime = with(case0802, log(Time))
histogram(~logtime, type = "density", density = TRUE, nint = 10, data = case0802)

histogram(~Voltage, type = "density", density = TRUE, nint = 10, data = case0802)

xyplot(logtime ~ Voltage, groups = Group, auto.key = TRUE, data = case0802)

lm1 = lm(logtime ~ Voltage, data = case0802)
summary(lm1)

exp(coef(lm1))
confint(lm1)
exp(confint(lm1))

anova(lm1)
lm2 = lm(logtime ~ as.factor(Voltage), data = case0802)
summary(lm2)

anova(lm2)
# Lack of fit test:
anova(lm1, lm2)

plot(lm1, which = 2)

plot(lm1, which = 1)

case0802$sqrttime = with(case0802, sqrt(Time))
xyplot(sqrttime ~ Voltage, type = c("p", "r"), data = case0802)

lm3 = lm(sqrttime ~ Voltage, data = case0802)
summary(lm3)
plot(lm3, which = 1)
