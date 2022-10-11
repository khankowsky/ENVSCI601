# RScs0901_2nd_Horton

install.packages("mosaic") # note the quotation marks
require(mosaic)
install.packages("Sleuth2") # note the quotation marks
require(Sleuth2)
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 4)

data(case0901)
fix(case0901)
summary(case0901)

favstats(Flowers ~ Intens | Time, data = case0901)

xyplot(Flowers ~ Intens, groups = Time, type = c("p", "r", "smooth"),
       data = case0901,
auto.key = TRUE, xlab = "light intensity (mu mol/m^2/sec)", 
ylab = "average number of flowers" )

lm1 = lm(Flowers ~ Intens + Time, data = case0901)
summary(lm1)
confint(lm1, level = 0.95) # 95% confidence intervals

lm2 = lm(Flowers ~ Intens*Time, data = case0901)
summary(lm2)
confint(lm2, level = 0.95) # 95% confidence intervals

