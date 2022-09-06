# Globe Tossing, written by Eugene.Gallagher@umb.edu, revised 9/4/22
# R code for Gelman & Nolan and McElreath and Kurz's globe tossing example

setwd("M:/EnvSci601/R/R_projects_F22/Wk01_IntroR_Ch01_F22/docs")

# All of these may not be needed.
library(binom)
library(car)
library(geosphere)
library(lattice)
library(sf)
library(spData)
library(tidyverse)


# Globe_water. Analysis of Land/Water Globe toss
# Read the data from Excel as a tibble

d<-read_csv("../data/Globe_Toss.csv", col_names=TRUE, show_col_types = FALSE)  # csv delimited text
glimpse(d)

(
  d <-
    d %>% 
    mutate(n_trials  = 1:nrow(d),
           n_success = cumsum(toss == "W"),
           p_success = n_success/n_trials)
)
glimpse(d)
# Calculate the binomial proportions and probability
# that the globe tossing differs from the known proportion of 0.71
n<-nrow(d)
p_val<-prop.test(d$n_success[n],d$n_trials[n],0.71)
p_val

# There is some controversy about the appropriate 95% CI in frequentist statistics
# one of many intervals produced by the binom pacakge
cis_all<-binom.confint(d$n_success[n],d$n_trials[n], conf.level = 0.95)
cis_all

## Generate 36 random coordinates for Google maps

# Generate random coordinates to to determine Water or land in 100 trials
# Random global coordinates using geosphere
# from https://rdrr.io/cran/geosphere/src/R/randomCoordinates.R

help(randomCoordinates)
# ??regularCoordinates
d1 <- randomCoordinates(36)
d2<-d1[,c(2,1)]  # swap latitudes and longitudes
d2
# Also see: https://www.random.org/geographic-coordinates/

## Generate 7910 to 9604 random coordinates and check 'margins of error' (half 95% CI)

# Using Gallagher's LMtheorem050302_6th.m, a Matlab file, n = 7910 or n = 9604 (worst case)

# How many samples needed for a binomial margin of error = 0.01
alpha <- 0.05
Psn <- 1-alpha/2  # cumulative probability function for alpha = 0.05
p <- 0.71 # expected probability
me <- 0.01 # margin of error
# see Equation 5.3.4., p 304, Larsen & Marx Intro. Math. Stat. 6th ed
n <- qnorm(Psn)^2*p*(1-p)/me^2
n

GlobeTosses <- n
d1<-randomCoordinates(GlobeTosses)
d2<-d1[,c(2,1)]  # swap latitudes and longitudes
# stack exchange
# https://gis.stackexchange.com/questions/75033/given-lat-and-lon-identify-if-point-over-land-or-ocean-using-r

# view the coordinates in an interactive map
## Create an sf POINTS object
Lat <- d2[,1]
Lon <- d2[,2]
# points <- expand.grid(Lon, Lat)  # Note that I reversed ordering of lat/long
points <- tibble(Lon,Lat)
pts <- st_as_sf(points, coords=1:2, crs=4326)

## Find which points fall over land
ii <- !is.na(as.numeric(st_intersects(pts, world)))

## Check that it worked
plot(st_geometry(world))
plot(pts, col=1+ii, pch=16, add=TRUE)

n_land <- sum(ii)
n_water <- GlobeTosses-n_land
cis_all<-binom.confint(n_water,GlobeTosses, conf.level = 0.95)
cis_all

# These confidence intervals have a maring of error of 0.01, as required
# 
# Now with the conservative estimate of the effective sample size, 9604, which
# assumes that p0= 0.5
p <- 0.5 # expected probability, most conservative, largest variance
# see Equation 5.3.4., p 304, Larsen & Marx Intro. Math. Stat. 6th ed
GlobeTosses <- qnorm(Psn)^2*p*(1-p)/me^2
# GlobeTosses <- 9604
d1<-randomCoordinates(GlobeTosses)
d2<-d1[,c(2,1)]  # swap latitudes and longitudes
# stack exchange
# https://gis.stackexchange.com/questions/75033/given-lat-and-lon-identify-if-point-over-land-or-ocean-using-r
# view the coordinates in an interactive map

## Create an sf POINTS object
Lat <- d2[,1]
Lon <- d2[,2]
# points <- expand.grid(Lon, Lat)  # Note that I reversed OP's ordering of lat/long
points <-tibble(Lon,Lat)
pts <- st_as_sf(points, coords=1:2, crs=4326)

## Find which points fall over land
ii <- !is.na(as.numeric(st_intersects(pts, world)))

## Check that it worked
plot(st_geometry(world))
plot(pts, col=1+ii, pch=16, add=TRUE)

n_land <- sum(ii)
n_water <- GlobeTosses-n_land
cis_all<-binom.confint(n_water,GlobeTosses, conf.level = 0.95)
cis_all
# These margins of error are 0.009, not 0.01

## What does R power analysis say?

# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/propTestN
library(EnvStats)
pow<-propTestN(p.or.p1 =0.72, p0.or.p2 = 0.71, alpha = 0.05, power = 0.95, 
               sample.type = "one.sample", alternative = "two.sided", 
               approx = FALSE, 
               round.up = TRUE, warn = TRUE, return.exact.list = TRUE, 
               n.min = 1000, n.max = 50000, 
               tol = 1e-8, maxiter = 10000)


GlobeTosses <- pow$n
d1<-randomCoordinates(GlobeTosses)
d2<-d1[,c(2,1)]  # swap latitudes and longitudes
# stack exchange
# https://gis.stackexchange.com/questions/75033/given-lat-and-lon-identify-if-point-over-land-or-ocean-using-r
# view the coordinates in an interactive map
 ## For `world`, an sf MULTIPOLYGON object

## Create an sf POINTS object
Lat <- d2[,1]
Lon <- d2[,2]
# points <- expand.grid(Lon, Lat)  # Note that I reversed OP's ordering of lat/long
points <-tibble(Lon,Lat)
pts <- st_as_sf(points, coords=1:2, crs=4326)

## Find which points fall over land
ii <- !is.na(as.numeric(st_intersects(pts, world)))

## Check that it worked
plot(st_geometry(world))
plot(pts, col=1+ii, pch=16, add=TRUE)

n_land <- sum(ii)
n_water <- GlobeTosses-n_land
cis_all<-binom.confint(n_water,GlobeTosses, conf.level = 0.95)
cis_all

# the model predicts TOO many samples to produce 0.01 margins of error.
# As shown previously, and by Gallagher's Matlab program, the correct value
# is 7910, NOT 26,504.


# Bayesian 95% credibility levels -----------------------------------------

# Now calculate the Bayesian Posterior using McElreath & Kurz code --------

# code based on Simon Kurz 
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/. Section 2.2

# Now plot the Bayesian prior and posterior distributions. This is tricky!
sequence_length <- 50

d %>% 
  expand(nesting(n_trials, toss, n_success), 
         p_water = seq(from = 0, to = 1, length.out = sequence_length)) %>% 
  group_by(p_water) %>% 
  # you can learn more about lagging here: 
  # https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lag '
  # or here: https://dplyr.tidyverse.org/reference/lead-lag.html
  mutate(lagged_n_trials  = lag(n_trials,  k = 1),
         lagged_n_success = lag(n_success, k = 1)) %>% 
  ungroup() %>% 
  mutate(prior      = ifelse(n_trials == 1, .5,
                             dbinom(x    = lagged_n_success, 
                                    size = lagged_n_trials, 
                                    prob = p_water)),
         likelihood = dbinom(x    = n_success, 
                             size = n_trials, 
                             prob = p_water),
         strip      = str_c("n = ", n_trials)
  ) %>% 
  # the next three lines allow us to normalize the prior and the likelihood, 
  # putting them both in a probability metric 
  group_by(n_trials) %>% 
  mutate(prior      = prior      / sum(prior),
         likelihood = likelihood / sum(likelihood)) %>%   
  
  # plot!
  ggplot(aes(x = p_water)) +
  geom_line(aes(y = prior), linetype = 2) +
  geom_line(aes(y = likelihood)) +
  scale_x_continuous("proportion water", breaks = c(0, .5, 1)) +
  scale_y_continuous("plausibility", breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~strip, scales = "free_y")

