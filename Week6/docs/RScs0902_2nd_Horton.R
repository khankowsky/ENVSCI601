# RScs0902_2nd_Horton

install.packages("mosaic") # note the quotation marks
require(mosaic)
install.packages("Sleuth2") # note the quotation marks
require(Sleuth2)
trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 4)

data(case0902)
summary(case0902)
case0902$logbrain = log(case0902$Brain)
case0902$logbody = log(case0902$Body)
case0902$loggest = log(case0902$Gestation)
case0902$loglitter = log(case0902$Litter)

summary(case0902)

pairs(case0902[c("Brain", "Body", "Gestation", "Litter")])

panel.hist = function(x, ...) {
usr = par("usr")
on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5))
h = hist(x, plot = FALSE)
breaks = h$breaks
nB = length(breaks)
y = h$counts
y = y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.lm = function(x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1,
                       col.lm = "red", ...) {
 points(x, y, pch = pch, col = col, bg = bg, cex = cex)
 ok = is.finite(x) & is.finite(y)
 if (any(ok))

 abline(lm(y[ok] ~ x[ok]))
}

pairs(~Brain + Body + Gestation + Litter, lower.panel = panel.smooth, 
    diag.panel = panel.hist, upper.panel = panel.lm, data = case0902)

pairs(~logbrain + logbody + loggest + loglitter, lower.panel = panel.smooth,
       diag.panel = panel.hist, upper.panel = panel.lm, data = case0902)

xyplot(logbrain ~ jitter(loglitter), data = case0902)

xyplot(Brain ~ jitter(Litter), scales = list(y = list(log = TRUE), 
                                             x = list(log = TRUE)),
          data = case0902)

case0902$weightcut = cut(case0902$Body, breaks = c(0, 2.1, 9.1, 100, 4200),
labels = c("Body Weight: 0kg to 2.1kg", "Body Weight: 2.1kg to 9.1kg", 
"Body Weight: 9.1kg to 100kg", "Body Weight: 100 to 4,200"))

xyplot(Brain ~ jitter(Litter) | weightcut, scales = list(y = list(log = TRUE),
    x = list(log = TRUE) ), type = c("p", "r"), data = case0902)

lm1 = lm(logbrain ~ logbody + loggest + loglitter, data = case0902)
summary(lm1)
