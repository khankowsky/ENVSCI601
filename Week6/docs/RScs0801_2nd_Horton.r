# RScs0801_2nd_Horton

install.packages("mosaic") # note the quotation marks
require(mosaic)
install.packages("Sleuth2") # note the quotation marks
require(Sleuth2)
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 4)

case0801
summary(case0801)

xyplot(Species ~ Area, data = case0801)

densityplot(~Area, data = case0801)
densityplot(~Species, data = case0801)

case0801$logarea = with(case0801, log(Area))
case0801$logspecies = with(case0801, log(Species))

xyplot(logspecies ~ logarea, type = c("p", "r"), data = case0801)

lm1 = lm(logspecies ~ logarea, data = case0801)
summary(lm1)
confint(lm1)
2^confint(lm1)

plot(lm1, which = 2)

plot(lm1, which = 1)

